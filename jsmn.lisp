;;;; jsmn.lisp — Faithful Common Lisp port of jsmn
;;;; Author: misakawasaki
;;;; License: MIT
;;;; Version: 1.0.0

(in-package :cl-jsmn)

;; ---------------------------------------------------------------------------
;; Global Optimization
;; ---------------------------------------------------------------------------

;; Optimize for execution speed, but keep safety at 1 to ensure 
;; array bounds checks remain active (crucial for parsers).
(declaim (optimize (speed 3) (safety 1) (debug 0)))

;; ---------------------------------------------------------------------------
;; Errors
;; ---------------------------------------------------------------------------

(define-condition jsmn-error (simple-error) ()
  (:documentation "Base condition for all JSMN errors."))

(define-condition invalid-json (jsmn-error) ()
  (:default-initargs :format-control "Invalid character inside JSON string.")
  (:documentation "Signaled when the parser encounters an unexpected character."))

(define-condition not-enough-tokens (jsmn-error) ()
  (:default-initargs :format-control "Not enough tokens provided.")
  (:documentation "Signaled when the token buffer is full."))

(define-condition incomplete-json (jsmn-error) ()
  (:default-initargs :format-control "Incomplete JSON.")
  (:documentation "Signaled when the JSON string ends prematurely."))

;; ---------------------------------------------------------------------------
;; Data Structures
;; ---------------------------------------------------------------------------

(deftype token-type ()
  '(member :undefined :object :array :string :primitive))

(defstruct token
  "Represents a single JSON node.
START and END are indices into the JSON string representing the range [start, end)."
  (type  :undefined :type token-type)
  (start -1         :type fixnum)
  (end   -1         :type fixnum)
  (size  0          :type fixnum))

(defstruct parser
  "Holds the state of the JSON parser."
  (pos      0 :type fixnum)   ;; Current offset in the JSON string
  (toknext  0 :type fixnum)   ;; Next available token index
  (toksuper -1 :type fixnum)) ;; Index of the current parent token

(defun reset-parser (parser)
  "Resets the parser state to the beginning, allowing reuse."
  (declare (type parser parser))
  (setf (parser-pos parser) 0
        (parser-toknext parser) 0
        (parser-toksuper parser) -1)
  parser)

;; ---------------------------------------------------------------------------
;; Internal Helpers
;; ---------------------------------------------------------------------------

(declaim (inline strict-whitespace-p))
(defun strict-whitespace-p (ch)
  "Returns T if CH is a valid JSON whitespace character.
Uses load-time-value to create a static search string for zero-allocation."
  (find ch (load-time-value (map 'string #'identity '(#\Space #\Tab #\Newline #\Return)))))

(declaim (inline match-string-p))
(defun match-string-p (json len pos target)
  "Returns T if string TARGET matches JSON starting at POS, checking bounds against LEN."
  (declare (type string json target) (type fixnum len pos))
  (let ((end (+ pos (length target))))
    (declare (type fixnum end))
    (and (<= end len)
         (string= json target :start1 pos :end1 end))))

(declaim (inline ignore-whitespace))
(defun ignore-whitespace (parser json len)
  "Advances the parser past any whitespace characters."
  (declare (type parser parser) (type string json) (type fixnum len))
  (loop while (< (parser-pos parser) len)
        while (strict-whitespace-p (char json (parser-pos parser)))
        do (incf (parser-pos parser))))

(declaim (inline consume-char))
(defun consume-char (parser json len expected)
  "Consumes exactly one character EXPECTED. Signals error if mismatch."
  (declare (type parser parser) (type string json) (type fixnum len) (type character expected))
  (when (or (>= (parser-pos parser) len)
            (char/= (char json (parser-pos parser)) expected))
    (error 'invalid-json))
  (incf (parser-pos parser)))

(defun alloc-token (parser tokens)
  "Allocates the next available token from the vector, and registers it with the current parent (toksuper).
Signals NOT-ENOUGH-TOKENS if the vector is full. If TOKENS is nil, simply counts the token and return nil."
  (declare (type parser parser) (type (or null (vector token)) tokens))
  (let ((i (parser-toknext parser))
	(super-idx (parser-toksuper parser)))
    (declare (type fixnum i super-idx))

    ;; Only check bounds if we are actually storing tokens
    (when (and tokens (>= i (length tokens)))
      (error 'not-enough-tokens))

    ;; Link to parent: If we have a parent, increment its size.
    ;; Only possible if we have a vector to update.
    (when (and tokens (/= super-idx -1))
      (incf (token-size (aref tokens super-idx))))

    (incf (parser-toknext parser))

    (if tokens
        (aref tokens i)
	nil)))

(defun fill-token (token type start end)
  "Populates the fields of a token structure. Does nothing if TOKEN is nil."
  (declare (type (or null token) token) (type token-type type) (type fixnum start end))
  (when token
    (setf (token-type token) type
          (token-start token) start
          (token-end token) end))
  token)

;; ---------------------------------------------------------------------------
;; Macros
;; ---------------------------------------------------------------------------

(defmacro with-json-accessors ((parser json) &body body)
  "Binds POS to the parser position and CH to the current character."
  `(symbol-macrolet ((pos (parser-pos ,parser))
		     (ch  (char ,json (parser-pos ,parser))))
     ,@body))

(defmacro with-parent-tracking ((parser new-super-index) &body body)
  "Save current toksuper, sets it to the NEW-SUPER-INDEX, executes BODY, then restores toksuper."
  (let ((old-super (gensym "OLD-SUPER")))
    `(let ((,old-super (parser-toksuper ,parser)))
       (setf (parser-toksuper ,parser) ,new-super-index)
       (multiple-value-prog1
	   (progn ,@body)
         (setf (parser-toksuper ,parser) ,old-super)))))

(defmacro with-collection-parsing ((parser json len end-char) &body body)
  "Macro to handle the loop logic for Arrays and Objects.
   Enforces strict comma separation between items."
  `(with-json-accessors (,parser ,json)
     (ignore-whitespace ,parser ,json ,len)

     ;; 1. Check for Empty Collection "[]" or "{}"
     (if (and (< pos ,len) (char= ch ,end-char))
         (incf pos) ;; Consumed closing char

         ;; 2. Non-Empty: Loop Items
         (loop
           ;; Run the body (parse-value or parse-item)
           ,@body

           (ignore-whitespace ,parser ,json ,len)

           ;; Safety check inside loop
           (when (>= pos ,len)
             (error 'incomplete-json))
  
	   (cond
             ;; Case A: Found Closing Char -> Done
             ((char= ch ,end-char) (incf pos) (return))

             ;; Case B: Found Comma -> Continue to next item
             ((char= ch #\,) (incf pos) (ignore-whitespace ,parser ,json, len))

             ;; Case C: No comma and no closing char -> strict error
             (t (error 'invalid-json)))))))

;; ---------------------------------------------------------------------------
;; Parsing Logic
;; ---------------------------------------------------------------------------

(defun parse-primitive (parser json len tokens)
  "Parses primitives with STRICT JSON number validation."
  (declare (type parser parser) (type string json) (type fixnum len) (type (or null (vector token)) tokens))
  (let ((start (parser-pos parser))
        (token (alloc-token parser tokens)))
    (declare (type fixnum start))

    (with-json-accessors (parser json)
      ;; 1. Check Keywords first (same as before)
      (cond
        ((match-string-p json len pos "true")  (incf pos 4))
        ((match-string-p json len pos "false") (incf pos 5))
        ((match-string-p json len pos "null")  (incf pos 4))

        ;; 2. Strict Number Parsing
        (t
         ;; A. Optional Minus (Start)
         (when (and (< pos len) (char= ch #\-))
           (incf pos))

         ;; B. Integer Part
         (if (>= pos len) (error 'invalid-json)) ;; Empty after minus?
     
         (cond
           ;; Leading Zero (Allowed, but next char cannot be a digit)
           ((char= ch #\0)
            (incf pos)
            ;; Strict check: 01 is invalid. 0.1 is valid. 0e1 is valid.
            (when (and (< pos len) (digit-char-p ch))
              (error 'invalid-json)))

           ;; Non-Zero Digit (1-9)
           ((digit-char-p ch)
            (loop while (and (< pos len) (digit-char-p ch))
                  do (incf pos)))

           ;; Anything else at start (e.g. "+", ".", "e") -> Error
           (t (error 'invalid-json)))

         ;; C. Fraction Part (Optional)
         (when (and (< pos len) (char= ch #\.))
           (incf pos) ;; Consume dot

         ;; Must have at least one digit after dot
         (when (or (>= pos len) (not (digit-char-p ch)))
           (error 'invalid-json))

         ;; Consume remaining digits
         (loop while (and (< pos len) (digit-char-p ch))
               do (incf pos)))

         ;; D. Exponent Part (Optional)
         (when (and (< pos len) (or (char= ch #\e) (char= ch #\E)))
           (incf pos) ;; Consume E

           ;; Optional Sign (+ or -) allowed ONLY here
           (when (and (< pos len) (or (char= ch #\+) (char= ch #\-)))
             (incf pos))

           ;; Must have at least one digit after E (and optional sign)
           (when (or (>= pos len) (not (digit-char-p ch)))
             (error 'invalid-json))

           ;; Consume remaining digits
           (loop while (and (< pos len) (digit-char-p ch))
                 do (incf pos))))))

    (fill-token token :primitive start (parser-pos parser))))

(defun parse-string (parser json len tokens)
  "Parses a quoted string, handling escaped characters."
  (declare (type parser parser) (type string json) (type fixnum len) (type (or null (vector token)) tokens))
  (let ((start (parser-pos parser))
        (token (alloc-token parser tokens)))
    (declare (type fixnum start))
    
    (consume-char parser json len #\")
   
    (with-json-accessors (parser json)
      (loop
        (when (>= pos len)
          (error 'incomplete-json))
  
        (cond 
          ;; Case 1: Escape Sequence (e.g. \") -> Skip backslash AND next char
          ((char= ch #\\) 
	   (incf pos)  ;; Skip backslash
	   (when (>= pos len) (error 'incomplete-json))
	   
	   ;; Strict validation of the charactor following backslash
	   (case ch
	     ;; Standard JSON escapes
	     ((#\" #\/ #\\ #\b #\f #\r #\n #\t)
	      (incf pos))

	     ;; Unicode escape \uXXXX
	     (#\u
	      (incf pos) ;; Skip 'u'
	      ;; Loop 4 times for 4 hex digits
	      (dotimes (i 4)
		(when (>= pos len) (error 'incomplete-json))
		;; Check if current char is hex digit (0-9, a-f, A-F)
		(unless (digit-char-p ch 16)
		  (error 'invalid-json))
		(incf pos)))

             ;; Invalid escape sequence (e.g. \a, \z)
	     (t (error 'invalid-json)))) 
          
          ;; Case 2: Closing Quote -> Done
          ((char= ch #\") (incf pos) (return))
          
          ;; Case 3: Regular character -> Advance
          (t (incf pos)))))
           
    (fill-token token :string start (parser-pos parser))))

(defun parse-value (parser json len tokens)
  "Determines the type of the next value and dispatches to the correct parser."
  (declare (type parser parser) (type string json) (type fixnum len) (type (or null (vector token)) tokens))
  
  ;; Safety check: Are we at end of string?
  (when (>= (parser-pos parser) len)
    (error 'incomplete-json))
  
  (with-json-accessors (parser json)
    (cond
      ((char= ch #\") (parse-string parser json len tokens))
      ((char= ch #\{) (parse-object parser json len tokens))
      ((char= ch #\[) (parse-array parser json len tokens))
      (t              (parse-primitive parser json len tokens)))))

(defun parse-item (parser json len tokens)
  "Parses a Key:Value pair within an Object."
  (declare (type parser parser) (type string json) (type fixnum len) (type (or null (vector token)) tokens))
  
  ;; Capture the index of the NEXT token (which will be the Key)
  (let ((key-index (parser-toknext parser)))
    ;; 1. Parse Key (Parent = Object)
    (parse-string parser json len tokens)
    (ignore-whitespace parser json len)
    (consume-char parser json len #\:)      ;; Separator
    (ignore-whitespace parser json len)

    ;; 2. Parse Value (Parent = Key)
    (with-parent-tracking (parser key-index)
      (parse-value parser json len tokens))))

(defun parse-array (parser json len tokens)
  "Parses a JSON Array [...]."
  (declare (type parser parser) (type string json) (type fixnum len) (type (or null (vector token)) tokens))
  (let ((start (parser-pos parser))
	(token-index (parser-toknext parser))
        (token (alloc-token parser tokens)))
    (declare (type fixnum start token-index))
    (consume-char parser json len #\[)
    (with-parent-tracking (parser token-index)
      (with-collection-parsing (parser json len #\])
        (parse-value parser json len tokens)))
    (fill-token token :array start (parser-pos parser))))

(defun parse-object (parser json len tokens)
  "Parses a JSON Object {...}."
  (declare (type parser parser) (type string json) (type fixnum len) (type (or null (vector token)) tokens))
  (let ((start (parser-pos parser))
	(token-index (parser-toknext parser))
        (token (alloc-token parser tokens)))
    (declare (type fixnum start token-index))
    (consume-char parser json len #\{)
    (with-parent-tracking (parser token-index)
      (with-collection-parsing (parser json len #\})
        (parse-item parser json len tokens)))
    (fill-token token :object start (parser-pos parser))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defun parse (parser json tokens)
  "Parse a full JSON document (object or array).
   Returns the number of tokens used.
   Signals INVALID-JSON, NOT-ENOUGH-TOKENS, or INCOMPLETE-JSON on error."
  ;; Public API Validation
  (check-type parser parser)
  (check-type json string)
  (check-type tokens (or null (vector token))) ;; Allow nil or vector

  (let ((len (length json)))
    (declare (type fixnum len))

    (reset-parser parser)
    (ignore-whitespace parser json len)

    ;; 1. Basic Validation
    (when (>= (parser-pos parser) len)
      (error 'invalid-json))

    (parse-value parser json len tokens)

    ;; 3. Ensure no trailing garbage (strict JSON)
    (ignore-whitespace parser json len)
    (when (< (parser-pos parser) len)
      (error 'invalid-json))

    ;; Return number of tokens consumed
    (parser-toknext parser)))
