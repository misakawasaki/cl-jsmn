A faithful, high-performance Common Lisp port of [jsmn](https://github.com/zserge/jsmn), a minimalistic JSON tokenizer.

***Overview***

*jsmn.lisp* is designed to be robust, fast, and memory-efficient. Unlike most JSON parsers that construct a heavy Abstract Syntax Tree (AST) or Lisp objects (hash tables/lists) immediately, `jsmn.lisp` simply scans the JSON string and emits tokens indicating the type and location (start/end indices) of JSON nodes.

This approach allows you to:

* **Avoid Allocation**: Re-use the same token vector and parser state for multiple files.

* **Parse Partially**: extract only the fields you need without decoding the entire object.

* **Validate Quickly**: Check if a JSON string is valid without constructing objects.

***Features***

1. **Zero Heap Allocation** (during parsing): Works entirely on the stack and the pre-allocated token vector.

2. **Strict JSON Compliance**: Enforces correct comma placement (unlike the original C jsmn in some modes) and handles escaped strings correctly.

3. **High Performance**: Heavily optimized with type declarations and inlining `((optimize (speed 3) (safety 1)))`.

4. **No Dependencies**: Pure Common Lisp.

***Optional Features***

By default, tokens store their `size` (number of children) but not a pointer to their parent, to save memory.
To enable the `parent` field (compatible with `JSMN_PARENT_LINKS`), push `:jsmn-parent-links` to your features list *before* loading the system.
```Lisp
(pushnew :jsmn-parent-links *features*)
(ql:quickload :jsmn)
```

***Installation***

Clone this repository into your Quicklisp `local-projects` directory:
```Shell
cd ~/quicklisp/local-projects
git clone https://github.com/misakawasaki/cl-jsmn
```

Load it via Quicklisp:
```Lisp
(ql:quickload :cl-jsmn)
```

Quick Start

Because *jsmn* is a tokenizer, it requires you to manage the memory for the tokens.
```Lisp
(use-package :cl-jsmn)

;; 1. Prepare Data
(defparameter *json* "{\"user\": \"admin\", \"id\": 1234, \"roles\": [\"read\", \"write\"]}")

;; 2. Allocate Parser and Token Vector (Reuse these!)
(defparameter *parser* (make-parser))
(defparameter *tokens* (make-array 128 :element-type 'token))

;; Initialize tokens (required once, as make-array creates nil/0 values)
(dotimes (i 128)
  (setf (aref *tokens* i) (make-token)))

;; 3. Parse
(handler-case
    (let ((count (parse *parser* *json* *tokens*)))
      (format t "Parsed ~A tokens:~%" count)
      
      ;; 4. Inspect Tokens
      (dotimes (i count)
        (let ((tok (aref *tokens* i)))
          (format t "Token ~D: Type=~A, Range=[~D, ~D), Content='~A'~%"
                  i
                  (token-type tok)
                  (token-start tok)
                  (token-end tok)
                  (subseq *json* (token-start tok) (token-end tok))))))
  
  ;; Error Handling
  (jsmn-error (e)
    (format t "Parsing failed: ~A~%" e)))
```
Output
```
Parsed 9 tokens:
Token 0: Type=OBJECT, Range=[0, 57), Content='{"user": "admin", "id": 1234, "roles": ["read", "write"]}'
Token 1: Type=STRING, Range=[1, 7), Content='"user"'
Token 2: Type=STRING, Range=[9, 16), Content='"admin"'
Token 3: Type=STRING, Range=[18, 22), Content='"id"'
Token 4: Type=PRIMITIVE, Range=[24, 28), Content='1234'
Token 5: Type=STRING, Range=[30, 37), Content='"roles"'
Token 6: Type=ARRAY, Range=[39, 56), Content='["read", "write"]'
Token 7: Type=STRING, Range=[40, 46), Content='"read"'
Token 8: Type=STRING, Range=[48, 55), Content='"write"'
```

Note: `jsmn` does not parse recursively in the traditional sense. It produces a flat array of tokens. The hierarchy is implied by the order.

***API Reference***

****Main Functions****
```Lisp
(parse parser json-string token-vector) => count
```
Parses the JSON string and fills the token vector.

* **Returns**: Number of tokens used.

* **Signals**:

    * `invalid-json`: Malformed JSON or unexpected characters.
    * `not-enough-tokens`: The provided vector is too small.
    * `incomplete-json`: The string ended prematurely.

```Lisp
(reset-parser parser) => parser
```
Resets the parser state (position and token index) to 0. Call this before parsing a new string with an existing parser instance.

****Structures****

`parser`

Holds the parsing state.

* `(make-parser)`: Create a new parser.

* `parser-pos`: Current character index in the string.

* `parser-toknext`: Index of the next token to allocate.

`token`

Represents a JSON node.

* `(make-token)`: Create a new token.

* `token-type`: Keyword `(:object | :array | :string | :primitive | :undefined)`.

* `token-start`: Start index in the JSON string.

* `token-end`: End index in the JSON string (exclusive).

* `token-size`: Number of child elements (for Objects and Arrays). For string tokens that serve as object keys, `size` is 1, indicating one associated value follows
* `token-parent`: (*Optional*) Index of the parent token. Available only if `:jsmn-parent-links` is enabled.

****Testing****

To run the test suite:
```Lisp
(ql:quickload :cl-jsmn/test)
(asdf:test-system :cl-jsmn)
```

****License****

MIT License.
