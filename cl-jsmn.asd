;;;; cl-jsmn.asd

(asdf:defsystem #:cl-jsmn
  :version "1.0.0"
  :author "misakawasaki"
  :license "MIT"
  :description "A minimal, jsmn-compatible JSON tokenizer for Common Lisp."
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :serial t ;; ensures 'package' loads before 'jsmn'
  :components ((:file "packages")
               (:file "jsmn"))
  :in-order-to ((test-op (test-op #:cl-jsmn/test))))

(asdf:defsystem #:cl-jsmn/test
  :description "Test suite for jsmn"
  :depends-on (#:cl-jsmn #:fiveam)
  :components ((:module "test"
		:serial t
		:components ((:file "packages")
			     (:file "tests"))))
  :perform (test-op (o c) (symbol-call :fiveam :run! :jsmn-tests)))
