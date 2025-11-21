;;;; jsmn.lisp — Faithful Common Lisp port of jsmn
;;;; Author: misakawasaki
;;;; License: MIT
;;;; Version: 1.0.0

(in-package :cl-jsmn)

;; ---------------------------------------------------------------------------
;; Global Optimization & Types
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
  (pos      0 :type fixnum) ;; Current offset in the JSON string
  (toknext  0 :type fixnum) ;; Next available token index
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
  "Allocates the next available token from the vector.
Signals NOT-ENOUGH-TOKENS if the vector is full."
  (declare (type parser parser) (type (vector token) tokens))
  (let ((i (parser-toknext parser)))
    (declare (type fixnum i))
    (when (>= i (length tokens))
      (error 'not-enough-tokens))
    (incf (parser-toknext parser))
    (aref tokens i)))

(defun fill-token (token type start end)
  "Populates the fields of a token structure."
  (declare (type token token) (type token-type type) (type fixnum start end))
  (setf (token-type token) type
        (token-start token) start
        (token-end token) end)
  token)

;; ---------------------------------------------------------------------------
;; Parsing Logic
;; ---------------------------------------------------------------------------

(defun parse-primitive (parser json len tokens)
  "Parses primitives: numbers, boolean (true/false), or null."
  (declare (type parser parser) (type string json) (type fixnum len) (type (vector token) tokens))
  (let ((start (parser-pos parser))
        (token (alloc-token parser tokens)))
    (declare (type fixnum start))
    
    ;; 1. Check for Keywords
    (cond
      ((match-string-p json len (parser-pos parser) "true")
       (incf (parser-pos parser) 4))
      ((match-string-p json len (parser-pos parser) "false")
       (incf (parser-pos parser) 5))
      ((match-string-p json len (parser-pos parser) "null")
       (incf (parser-pos parser) 4))
      (t
       ;; 2. Check for Numbers
       (loop while (< (parser-pos parser) len)
             for ch = (char json (parser-pos parser))
             while (or (alphanumericp ch) (find ch "+- .eE"))
             do (incf (parser-pos parser)))
       
       ;; 3. Validation
       (when (= (parser-pos parser) start)
         (error 'invalid-json))))
    
    (fill-token token :primitive start (parser-pos parser))))

(defun parse-string (parser json len tokens)
  "Parses a quoted string, handling escaped characters."
  (declare (type parser parser) (type string json) (type fixnum len) (type (vector token) tokens))
  (let ((start (parser-pos parser))
        (token (alloc-token parser tokens)))
    (declare (type fixnum start))
    
    (consume-char parser json len #\")
    
    (loop
      (when (>= (parser-pos parser) len)
        (error 'incomplete-json))
      (let ((ch (char json (parser-pos parser))))
        (cond 
          ;; Case 1: Escape Sequence (e.g. \") -> Skip backslash AND next char
          ((char= ch #\\)
           (incf (parser-pos parser) 2)) 
          
          ;; Case 2: Closing Quote -> Done
          ((char= ch #\")
           (incf (parser-pos parser))
           (return))
          
          ;; Case 3: Regular character -> Advance
          (t 
           (incf (parser-pos parser))))))
           
    (fill-token token :string start (parser-pos parser))))

(defun parse-value (parser json len tokens)
  "Determines the type of the next value and dispatches to the correct parser."
  (declare (type parser parser) (type string json) (type fixnum len) (type (vector token) tokens))
  
  ;; Safety check: Are we at end of string?
  (when (>= (parser-pos parser) len)
    (error 'incomplete-json))
    
  (let ((ch (char json (parser-pos parser))))
    (cond
      ((char= ch #\") (parse-string parser json len tokens))
      ((char= ch #\{) (parse-object parser json len tokens))
      ((char= ch #\[) (parse-array parser json len tokens))
      (t              (parse-primitive parser json len tokens)))))

(defun parse-item (parser json len tokens)
  "Parses a Key:Value pair within an Object."
  (declare (type parser parser) (type string json) (type fixnum len) (type (vector token) tokens))
  (parse-string parser json len tokens)   ;; Key
  (ignore-whitespace parser json len)
  (consume-char parser json len #\:)      ;; Separator
  (ignore-whitespace parser json len)
  (parse-value parser json len tokens))   ;; Value

(defmacro with-collection-parsing ((parser json len end-char) &body body)
  "Macro to handle the loop logic for Arrays and Objects.
   Enforces strict comma separation between items."
  `(progn
     (ignore-whitespace ,parser ,json ,len)
     
     ;; 1. Check for Empty Collection "[]" or "{}"
     (if (and (< (parser-pos ,parser) ,len)
              (char= (char ,json (parser-pos ,parser)) ,end-char))
         (incf (parser-pos ,parser)) ;; Consumed closing char
         
         ;; 2. Non-Empty: Loop Items
         (loop
           ;; Run the body (parse-value or parse-item)
           ,@body
           
           (ignore-whitespace ,parser ,json ,len)
           
           ;; Safety check inside loop
           (when (>= (parser-pos ,parser) ,len)
             (error 'incomplete-json))
             
           (let ((ch (char ,json (parser-pos ,parser))))
             (cond
               ;; Case A: Found Closing Char -> Done
               ((char= ch ,end-char)
                (incf (parser-pos ,parser))
                (return))
               
               ;; Case B: Found Comma -> Continue to next item
               ((char= ch #\,)
                (incf (parser-pos ,parser)))
               
               ;; Case C: No comma and no closing char -> strict error
               (t (error 'invalid-json))))))))

(defun parse-array (parser json len tokens)
  "Parses a JSON Array [...]."
  (declare (type parser parser) (type string json) (type fixnum len) (type (vector token) tokens))
  (let ((start (parser-pos parser))
        (token (alloc-token parser tokens)))
    (consume-char parser json len #\[)
    (with-collection-parsing (parser json len #\])
      (parse-value parser json len tokens))
    (fill-token token :array start (parser-pos parser))))

(defun parse-object (parser json len tokens)
  "Parses a JSON Object {...}."
  (declare (type parser parser) (type string json) (type fixnum len) (type (vector token) tokens))
  (let ((start (parser-pos parser))
        (token (alloc-token parser tokens)))
    (consume-char parser json len #\{)
    (with-collection-parsing (parser json len #\})
      (parse-item parser json len tokens))
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
  (check-type tokens (vector token))

  (let ((len (length json)))
    (declare (type fixnum len))
    
    (reset-parser parser)
    (ignore-whitespace parser json len)
    
    ;; 1. Basic Validation
    (when (>= (parser-pos parser) len)
      (error 'invalid-json))
    
    ;; 2. Determine Root Type
    (let ((ch (char json (parser-pos parser))))
      (cond
        ((char= ch #\{) (parse-object parser json len tokens))
        ((char= ch #\[) (parse-array  parser json len tokens))
        (t (error 'invalid-json))))
    
    ;; 3. Ensure no trailing garbage (strict JSON)
    (ignore-whitespace parser json len)
    (when (< (parser-pos parser) len)
      (error 'invalid-json))
    
    ;; Return number of tokens consumed
    (parser-toknext parser)))
