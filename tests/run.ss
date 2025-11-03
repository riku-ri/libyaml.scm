;; Only promise to test schemas listed in the section #Recommended schemas#
;; - https://yaml.org/spec/1.2.2/#recommended-schemas
;; WITHOUT TAG. yaml tag is undefined in libyaml.ss,
;;   and may make error

(import libyaml)
(import test)

(test-group "yaml<-"
	(test #t (procedure? (yaml<- "")))
	(test #t (procedure?
			(with-input-from-file "/dev/null"
				(lambda () (yaml<- (current-input-port))))))
	(test-group "varg"
		(test-group "#:encoding"
			(test #t (null? ((yaml<- `(#:encoding . ,YAML_ANY_ENCODING) ""))))
			(test #t (null? ((yaml<- `(#:encoding . ,YAML_UTF8_ENCODING) ""))))
			(test #t (string=? "测试" ((yaml<- `(#:encoding . ,YAML_UTF8_ENCODING) "测试"))))
			; does not test other encodings
			(test-error ((yaml<- `(#:encoding . -1) "")))
		)
	)
	(test-group "document"
		(test #t (procedure? (yaml<- "")))
		(test-error ((yaml<- "") 1))
		(test 2 ((yaml<- "--- 1\n--- 2") 1))
	)
	(test-group "scalar"
		(test-group "null"
			(test #t (null? ((yaml<- ""))))
			(test #t (null? ((yaml<- "~"))))
			(test #t (null? ((yaml<- "null"))))
			(test #t (null? ((yaml<- "Null"))))
			(test #t (null? ((yaml<- "NULL"))))
			(test #t (null?
				((with-input-from-file
					"/dev/null"
					(lambda () (yaml<- (current-input-port)))))))
		)
		(test-group "boolean"
			(test #t ((yaml<- "true")))
			(test #t ((yaml<- "True")))
			(test #t ((yaml<- "TRUE")))
			(test #f ((yaml<- "false")))
			(test #f ((yaml<- "False")))
			(test #f ((yaml<- "FALSE")))
		)
		(test-group "numeric"
			(test-group "integer"
				(let-syntax
					(
						(test-int (syntax-rules ()
							((test-int num str) (let ()
								(test #t (integer? ((yaml<- str))))
								(test num ((yaml<- str)))))))
						(test-int+- (syntax-rules ()
							((test-int num str) (let ()
								(test #t (integer? ((yaml<- (string-append "-" str)))))
								(test (- num) ((yaml<- (string-append "-" str))))
								(test #t (integer? ((yaml<- (string-append "+" str)))))
								(test (+ num) ((yaml<- (string-append "+" str))))))))
					)
					(test-int+- 1 "1")
					(test-int+- 0 "0")
					(test-int+- 64 "64")
					(test-int #o505 "0o505") (test-int 325 "0o505")
					(test "-0o112" ((yaml<- "-0o112")))
					(test "+0o112" ((yaml<- "+0o112")))
					(test-int #x505 "0x505") (test-int 1285 "0x505")
					(test "-0x112" ((yaml<- "-0x112")))
					(test "+0x112" ((yaml<- "+0x112")))
				)
			)
			(test-group "real"
				(let-syntax ((test-real+- (syntax-rules ()
					((test-real+- num str) (let ()
						;(test #t (real? ((yaml<- str))))
						(test num ((yaml<- str)))
						;(test #t (real? ((yaml<- (string-append "-" str)))))
						(test (- num) ((yaml<- (string-append "-" str))))
						;(test #t (real? ((yaml<- (string-append "+" str)))))
						(test (+ num) ((yaml<- (string-append "+" str))))
					)))))
					(test-real+- 0.0 "0.0")
					(test-real+- 0.1 "0.1")
					(test-real+- (exact->inexact 12000) "12e03")
					(test-real+- 0.001862 "1.862e-3")
					(test-real+- 12.4 "1.24E+1")
					(test-real+- (exact->inexact 3) "3E+0")
				)
			)
			(test-group "inf/nan"
				(test +inf.0 ((yaml<- "+.inf"))) (test +inf.0 ((yaml<- "+.INF")))
				(test +inf.0 ((yaml<- "+.Inf")))

				(test -inf.0 ((yaml<- "-.inf"))) (test -inf.0 ((yaml<- "-.INF")))
				(test -inf.0 ((yaml<- "-.Inf")))

				(test #t (nan? ((yaml<- ".nan")))) (test #t (nan? ((yaml<- ".NAN"))))
				(test #t (nan? ((yaml<- ".NaN"))))
			)
		)
		(test-group "string"
			(test "" ((yaml<- "''")))
			(test "" ((yaml<- "\"\"")))
			(test "here" ((yaml<- "here")))
			(test "@" ((yaml<- "\"\\x40\"")))
			(test "\\x40" ((yaml<- "\\x40")))
			(test "\n" ((yaml<- "\"\\n\"")))
			(test "line2\n" ((yaml<- "|\n  line2\n")))
			(test "line2\nL\n" ((yaml<- "|\n  line2\n  L\n")))
			(test "line2\nL" ((yaml<- "|-\n  line2\n  L\n")))
			(test "line2 L" ((yaml<- ">-\n  line2\n  L\n")))
			(test "line2 L\n" ((yaml<- ">\n  line2\n  L\n")))
		)
	) ; scalar
	(test-group "list"
		(test 0 (vector-length ((yaml<- "[]"))))
		(test "@" (vector-ref ((yaml<- "[\"\\x40\",2,'u']")) 0))
		(test "@"
			(vector-ref (vector-ref ((yaml<- "-\n  - \"\\x40\"")) 0) 0))
		(test '("key" . #("@"))
			(assoc "key" (car ((yaml<- "key:\n  - \"\\x40\"")))))
	)
	(test-group "map" (let ((mapv (lambda (key yamlmap) (cdr (assoc key (car yamlmap))))))
		(test 0 (length (car ((yaml<- "{}")))))
		(test "@" (mapv "key" ((yaml<- "{key: \"\\x40\"}"))))
		(test "@"
			(mapv "key" ((yaml<- "key:\n  \"\\x40\""))))
		(test (vector "@")
			(mapv '() (mapv "key"
				((yaml<- "key:\n  ~:\n    - \"\\x40\"")))))
	))
)
