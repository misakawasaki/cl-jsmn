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
;; Primitives
;; ---------------------------------------------------------------------------

(test parse-primitives
  (with-parser (p tokens "true")
    (is (= 1 (parser-toknext p)))
    (let ((tok (get-token tokens 0)))
      (is (eq :primitive (token-type tok)))
      (is (= 0 (token-start tok)))
      (is (= 4 (token-end tok)))))

  (with-parser (p tokens "false")
    (is (eq :primitive (token-type (get-token tokens 0)))))

  (with-parser (p tokens "null")
    (is (eq :primitive (token-type (get-token tokens 0)))))

  (with-parser (p tokens "123.45")
    (is (eq :primitive (token-type (get-token tokens 0))))
    (is (= 6 (token-end (get-token tokens 0))))))

;; ---------------------------------------------------------------------------
;; Strings
;; ---------------------------------------------------------------------------

(test parse-strings
  (with-parser (p tokens "\"Hello World\"")
    (is (= 1 (parser-toknext p)))
    (let ((tok (get-token tokens 0)))
      (is (eq :string (token-type tok)))
      (is (= 0 (token-start tok)))
      (is (= 13 (token-end tok))))) ;; Includes quotes in JSMN style

  ;; Test Escaped Quotes: "foo\"bar"
  (with-parser (p tokens "\"foo\\\"bar\"")
    (is (= 1 (parser-toknext p)))
    (let ((tok (get-token tokens 0)))
      (is (eq :string (token-type tok)))
      ;; Start quote (index 0) to End quote (index 9) -> length 10
      (is (= 10 (token-end tok)))))) 

;; ---------------------------------------------------------------------------
;; Arrays & Strict Commas
;; ---------------------------------------------------------------------------

(test parse-array-simple
  (with-parser (p tokens "[1, 2, 3]")
    ;; 1 Array + 3 Primitives = 4 Tokens
    (is (= 4 (parser-toknext p)))
    (is (eq :array (token-type (get-token tokens 0))))
    (is (eq :primitive (token-type (get-token tokens 1))))))

(test parse-array-empty
  (with-parser (p tokens "[]")
    (is (= 1 (parser-toknext p)))
    (is (eq :array (token-type (get-token tokens 0))))))

(test parse-array-strict-comma
  "Ensure that missing commas raise an error"
  (signals invalid-json
    (with-parser (p tokens "[1 2]") ;; Missing comma
      (parse p "[1 2]" tokens))))

(test parse-array-trailing-comma
  "JSMN usually allows trailing commas? Our strict port does NOT."
  (signals invalid-json
    (with-parser (p tokens "[1,]")
      (parse p "[1,]" tokens))))

;; ---------------------------------------------------------------------------
;; Objects
;; ---------------------------------------------------------------------------

(test parse-object-simple
  ;; {"key": 123}
  (with-parser (p tokens "{\"key\": 123}")
    ;; 1 Object + 1 String (Key) + 1 Primitive (Value) = 3 Tokens
    (is (= 3 (parser-toknext p)))
    (is (eq :object (token-type (get-token tokens 0))))
    (is (eq :string (token-type (get-token tokens 1))))
    (is (eq :primitive (token-type (get-token tokens 2))))))

(test parse-object-nested
  ;; {"a": [1]}
  (with-parser (p tokens "{\"a\": [1]}")
    ;; Obj(1) + Str(1) + Arr(1) + Prim(1) = 4 Tokens
    (is (= 4 (parser-toknext p)))
    (is (eq :object (token-type (get-token tokens 0))))
    (is (eq :string (token-type (get-token tokens 1)))) ;; Key "a"
    (is (eq :array  (token-type (get-token tokens 2)))) ;; Value [1]
    (is (eq :primitive (token-type (get-token tokens 3)))))) ;; Element 1

;; ---------------------------------------------------------------------------
;; Error Handling
;; ---------------------------------------------------------------------------

(test not-enough-tokens
  ;; Provide a tiny array of only 1 token
  (let ((p (make-parser))
        (tiny-tokens (make-array 1 :element-type 'token)))
    (setf (aref tiny-tokens 0) (make-token))
    
    ;; Try to parse "[1]" which needs 2 tokens (Array + Primitive)
    (signals not-enough-tokens
      (parse p "[1]" tiny-tokens))))

(test invalid-json-garbage
  (signals invalid-json
    (with-parser (p tokens "[1, ")
      (parse p "[1, " tokens))))

(test invalid-json-garbage
  (signals invalid-json
    (with-parser (p tokens "{\"a\": 1} garbage")
      (parse p "{\"a\": 1} garbage" tokens))))
