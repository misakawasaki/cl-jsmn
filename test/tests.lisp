;;;; tests.lisp

(in-package :cl-jsmn-test)

(def-suite :jsmn-tests
  :description "Tests for the JSMN port.")

(in-suite :jsmn-tests)

;; ---------------------------------------------------------------------------
;; Test Helpers
;; ---------------------------------------------------------------------------

(defparameter *token-buffer-size* 128)

(defmacro with-parser ((parser-var tokens-var input-str) &body body)
  "Sets up a parser and token vector for a test case."
  `(let ((,parser-var (make-parser))
         (,tokens-var (make-array *token-buffer-size* :element-type 'token)))
     ;; Initialize the array with empty tokens
     (dotimes (i *token-buffer-size*)
       (setf (aref ,tokens-var i) (make-token)))
     (let ((count (parse ,parser-var ,input-str ,tokens-var)))
       (declare (ignorable count))
       ,@body)))

(defun get-token (tokens index)
  (aref tokens index))

;; ---------------------------------------------------------------------------
;; Primitives & Strict Numbers
;; ---------------------------------------------------------------------------

(test parse-primitives-basic
  (with-parser (p tokens "true")
    (is (= 1 (parser-toknext p)))
    (let ((tok (get-token tokens 0)))
      (is (eq :primitive (token-type tok)))
      (is (= 0 (token-start tok)))
      (is (= 4 (token-end tok)))))

  (with-parser (p tokens "false")
    (is (eq :primitive (token-type (get-token tokens 0)))))

  (with-parser (p tokens "null")
    (is (eq :primitive (token-type (get-token tokens 0))))))

(test strict-numbers-valid
  "Test strict JSON number formatting (RFC 8259)."
  ;; Integers
  (with-parser (p tokens "0") (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "-0") (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "123") (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "-123") (is (= 1 (parser-toknext p))))
  
  ;; Floats
  (with-parser (p tokens "0.1") (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "-0.123") (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "123.456") (is (= 1 (parser-toknext p))))
  
  ;; Exponents
  (with-parser (p tokens "1e5") (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "1E+5") (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "1e-5") (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "0.5e10") (is (= 1 (parser-toknext p)))))

(test strict-numbers-invalid
  "Ensure invalid number formats signal invalid-json."
  ;; Leading zeros
  (signals invalid-json (with-parser (p tokens "01") (parse p "01" tokens)))
  (signals invalid-json (with-parser (p tokens "-01") (parse p "-01" tokens)))
  
  ;; Leading/Trailing dots
  (signals invalid-json (with-parser (p tokens ".1") (parse p ".1" tokens)))
  (signals invalid-json (with-parser (p tokens "1.") (parse p "1." tokens)))
  
  ;; Leading Plus (Not allowed in JSON)
  (signals invalid-json (with-parser (p tokens "+1") (parse p "+1" tokens)))
  
  ;; Malformed Exponents
  (signals invalid-json (with-parser (p tokens "1e") (parse p "1e" tokens)))
  (signals invalid-json (with-parser (p tokens "1e+") (parse p "1e+" tokens)))
  (signals invalid-json (with-parser (p tokens "1.e1") (parse p "1.e1" tokens))))

;; ---------------------------------------------------------------------------
;; Strict Strings & Escapes
;; ---------------------------------------------------------------------------

(test strict-strings-valid
  (with-parser (p tokens "\"Hello World\"")
    (is (= 1 (parser-toknext p))))
  
  ;; Standard Escapes
  (with-parser (p tokens "\"Line\\nFeed\"")
    (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "\"Quote\\\"Backslash\\\\\"")
    (is (= 1 (parser-toknext p))))
  
  ;; Unicode Escapes
  (with-parser (p tokens "\"Unicode \\u0041\"") ;; \u0041 is 'A'
    (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "\"Unicode \\u1234\"") 
    (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "\"Unicode \\uABCF\"") 
    (is (= 1 (parser-toknext p))))
  (with-parser (p tokens "\"Unicode \\uabcf\"") 
    (is (= 1 (parser-toknext p)))))

(test strict-strings-invalid
  ;; Unknown escape char
  (signals invalid-json 
    (with-parser (p tokens "\"Bad escape \\a\"") 
      (parse p "\"Bad escape \\a\"" tokens)))
  
  ;; Malformed Unicode
  (signals invalid-json 
    (with-parser (p tokens "\"Bad hex \\uZZZZ\"") 
      (parse p "\"Bad hex \\uZZZZ\"" tokens)))
  
  (signals invalid-json 
    (with-parser (p tokens "\"Short hex \\u123\"") 
      (parse p "\"Short hex \\u123\"" tokens)))
  
  (signals invalid-json
    (with-parser (p tokens "\"Cut off \\u12\"") 
      (parse p "\"Cut off \\u12\"" tokens))))

;; ---------------------------------------------------------------------------
;; Arrays & Strict Commas
;; ---------------------------------------------------------------------------

(test parse-array-simple
  (with-parser (p tokens "[1, 2, 3]")
    ;; 1 Array + 3 Primitives = 4 Tokens
    (is (= 4 (parser-toknext p)))
    (let ((arr (get-token tokens 0)))
      (is (eq :array (token-type arr)))
      (is (= 3 (token-size arr)))))) ;; Array Size = Number of Elements

(test parse-array-strict-comma
  "Ensure that missing commas raise an error"
  (signals invalid-json
    (with-parser (p tokens "[1 2]") ;; Missing comma
      (parse p "[1 2]" tokens))))

(test parse-array-trailing-comma
  "Ensure trailing commas are rejected (Strict JSON)."
  (signals invalid-json
    (with-parser (p tokens "[1,]")
      (parse p "[1,]" tokens))))

;; ---------------------------------------------------------------------------
;; Objects, Size & Hierarchy (v1.3.0 Logic)
;; ---------------------------------------------------------------------------

(test parse-object-simple
  ;; {"key": 123}
  (with-parser (p tokens "{\"key\": 123}")
    ;; 1 Object + 1 String (Key) + 1 Primitive (Value) = 3 Tokens
    (is (= 3 (parser-toknext p)))
    
    ;; Token 0: Object
    (let ((obj (get-token tokens 0)))
      (is (eq :object (token-type obj)))
      ;; v1.3.0 Change: Size = Number of KEYS (Pairs), not total children
      (is (= 1 (token-size obj))))
    
    ;; Token 1: Key "key"
    (let ((key (get-token tokens 1)))
      (is (eq :string (token-type key)))
      ;; v1.3.0 Change: Key is Parent of Value, so Size = 1
      (is (= 1 (token-size key))))
    
    ;; Token 2: Value 123
    (let ((val (get-token tokens 2)))
      (is (eq :primitive (token-type val)))
      (is (= 0 (token-size val))))))

(test parse-object-multiple
  ;; {"a": 1, "b": 2}
  (with-parser (p tokens "{\"a\": 1, \"b\": 2}")
    ;; Obj + KeyA + Val1 + KeyB + Val2 = 5 Tokens
    (is (= 5 (parser-toknext p)))
    
    (let ((obj (get-token tokens 0)))
      (is (= 2 (token-size obj)))) ;; 2 Pairs
      
    (let ((key-a (get-token tokens 1)))
      (is (= 1 (token-size key-a)))) ;; Key "a" has 1 child
      
    (let ((key-b (get-token tokens 3)))
      (is (= 1 (token-size key-b)))))) ;; Key "b" has 1 child

(test parse-object-nested
  ;; {"a": [1, 2]}
  (with-parser (p tokens "{\"a\": [1, 2]}")
    ;; Obj(1) + Key(1) + Arr(1) + Prim(1) + Prim(1) = 5 Tokens
    (is (= 5 (parser-toknext p)))
    
    ;; Object
    (is (eq :object (token-type (get-token tokens 0))))
    (is (= 1 (token-size (get-token tokens 0)))) ;; 1 Pair
    
    ;; Key "a"
    (is (eq :string (token-type (get-token tokens 1))))
    (is (= 1 (token-size (get-token tokens 1)))) ;; Key contains Array
    
    ;; Array [1, 2]
    (let ((arr (get-token tokens 2)))
      (is (eq :array (token-type arr)))
      (is (= 2 (token-size arr)))))) ;; Array contains 2 items

;; ---------------------------------------------------------------------------
;; Error Handling
;; ---------------------------------------------------------------------------

(test not-enough-tokens
  (let ((p (make-parser))
        (tiny-tokens (make-array 1 :element-type 'token)))
    (setf (aref tiny-tokens 0) (make-token))
    (signals not-enough-tokens
      (parse p "[1]" tiny-tokens))))

(test incomplete-json
  (signals incomplete-json
    (with-parser (p tokens "[1, ")
      (parse p "[1, " tokens))))

(test invalid-json-garbage
  (signals invalid-json
    (with-parser (p tokens "{\"a\": 1} garbage")
      (parse p "{\"a\": 1} garbage" tokens))))
;; ---------------------------------------------------------------------------
;; Counting Mode (No Allocation)
;; ---------------------------------------------------------------------------

(test counting-mode
  "Ensure parser correctly counts tokens when token buffer is NIL."
  (let ((p (make-parser)))
    ;; Primitive: true -> 1 token
    (reset-parser p)
    (is (= 1 (parse p "true" nil)))
    
    ;; Array: [1, 2] -> Array + 1 + 2 = 3 tokens
    (reset-parser p)
    (is (= 3 (parse p "[1, 2]" nil)))
    
    ;; Object: {"a": 1} -> Object + Key + Value = 3 tokens
    (reset-parser p)
    (is (= 3 (parse p "{\"a\": 1}" nil)))

    ;; Nested: {"a": [1, 2]} -> Obj + Key + Arr + 1 + 2 = 5 tokens
    (reset-parser p)
    (is (= 5 (parse p "{\"a\": [1, 2]}" nil)))))
;; ---------------------------------------------------------------------------
;; Optional Feature: Parent Links
;; ---------------------------------------------------------------------------

;; Only compile this test if the feature is enabled
#+jsmn-parent-links
(test check-parent-links
  "Verify that parent links are correctly set when :jsmn-parent-links is enabled."
  (with-parser (p tokens "{\"a\": [1]}")
    ;; Tokens:
    ;; 0: Object {"a": [1]} (Parent: -1)
    ;; 1: String "a"        (Parent: 0) -> Key is child of Object
    ;; 2: Array [1]         (Parent: 1) -> Value is child of Key (v1.3.0 logic)
    ;; 3: Primitive 1       (Parent: 2) -> Element is child of Array
    
    (is (= 4 (parse p "{\"a\": [1]}" tokens)))
    
    (let ((obj (get-token tokens 0))
          (key (get-token tokens 1))
          (arr (get-token tokens 2))
          (val (get-token tokens 3)))
          
      (is (= -1 (token-parent obj)))  ;; Root has no parent
      (is (= 0  (token-parent key)))  ;; Key -> Object
      (is (= 1  (token-parent arr)))  ;; Array -> Key
      (is (= 2  (token-parent val)))))) ;; Primitive -> Array
