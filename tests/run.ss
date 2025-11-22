;; Only promise to test schemas listed in the section #Recommended schemas#
;; - https://yaml.org/spec/1.2.2/#recommended-schemas
;; WITHOUT TAG. yaml tag is undefined in libyaml.ss,
;;   and may make error

(import libyaml)
(import test)

(define-syntax test? (syntax-rules ()
	((test? ?) (let ()
		(test #t ?)))))

(test-group "yaml<-"
	(test? (procedure? (yaml<- "")))
	(test? (procedure?
			(with-input-from-file "/dev/null"
				(lambda () (yaml<- (current-input-port))))))
	(test-group "varg"
		(test-group "#:encoding"
			(test? (null? ((yaml<- `(#:encoding . ,YAML_ANY_ENCODING) ""))))
			(test? (null? ((yaml<- `(#:encoding . ,YAML_UTF8_ENCODING) ""))))
			(test? (string=? "测试" ((yaml<- `(#:encoding . ,YAML_UTF8_ENCODING) "测试"))))
			; does not test other encodings
			(test-error ((yaml<- `(#:encoding . -1) "")))
		)
	)
	(test-group "document"
		(test? (procedure? (yaml<- "")))
		(test-error ((yaml<- "") 1))
		(test 2 ((yaml<- "--- 1\n--- 2") 1))
	)
	(test-group "scalar"
		(test-group "null"
			(test? (null? ((yaml<- ""))))
			(test? (null? ((yaml<- "~"))))
			(test? (null? ((yaml<- "null"))))
			(test? (null? ((yaml<- "Null"))))
			(test? (null? ((yaml<- "NULL"))))
			(test? (null?
				((with-input-from-file
					"/dev/null"
					(lambda () (yaml<- (current-input-port)))))))
		)
		(test-group "boolean"
			(test? ((yaml<- "true")))
			(test? ((yaml<- "True")))
			(test? ((yaml<- "TRUE")))
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
								(test? (integer? ((yaml<- str))))
								(test num ((yaml<- str)))))))
						(test-int+- (syntax-rules ()
							((test-int num str) (let ()
								(test? (integer? ((yaml<- (string-append "-" str)))))
								(test (- num) ((yaml<- (string-append "-" str))))
								(test? (integer? ((yaml<- (string-append "+" str)))))
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
						;(test? (real? ((yaml<- str))))
						(test num ((yaml<- str)))
						;(test? (real? ((yaml<- (string-append "-" str)))))
						(test (- num) ((yaml<- (string-append "-" str))))
						;(test? (real? ((yaml<- (string-append "+" str)))))
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

				(test? (nan? ((yaml<- ".nan")))) (test? (nan? ((yaml<- ".NAN"))))
				(test? (nan? ((yaml<- ".NaN"))))
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

(test-group "if.ss"
	(test-group "yscalar?"
		(test? (yscalar? '()))
		(test? (yscalar? #t)) (test? (yscalar? #f))
		(test? (yscalar? 0)) (test? (yscalar? 0.1)) (test? (yscalar? (/ 1 2)))
		(test? (yscalar? "")) (test? (yscalar? "关注电池耐用形色好谢谢喵"))
		(test? (not (yscalar? #:keyword)))
		(test? (not (yscalar? 'symbol)))
	)
	(test-group "ymap?"
		(test? (ymap? '(())))
		(test? (ymap? '(((0 . 1)))))
		(test? (ymap? '(((() . "null")))))
		(test? (not (ymap? '((("key" . false-here))))))
		(test? (not (ymap? '(((false-here . "value"))))))
		(test? (ymap? '(( (#() . (( ("key" . #("value" 0.1) ) ))) )) ))
	)
	(test-group "ylist?"
		(test? (ylist? #()))
		(test? (ylist? #(#())))
		(test? (ylist? #(1 "" #() (()))))
		(test? (ylist? #((( ((((0 . 1))) . #("u" "v")) )) "tail")))
		(test? (not (ylist? #(symbol))))
		(test? (not (ylist? #(#:keyword))))
	)
	(test-group "ydoc?"
		(test? (ydoc? '()))
		(test? (ydoc? '("after zero" 2 #(3 4) (( (() . #f) )))))
		(test? (ydoc? ((yaml<- "") -1)))
		(test? (ydoc?
			((with-input-from-file "/dev/null"
				(lambda () (yaml<- (current-input-port)))) -1)))
		(test? (not (ydoc? (yaml<- ""))))
		(test? (not (ydoc?
			(with-input-from-file "/dev/null"
				(lambda () (yaml<- (current-input-port)))))))
	)
	(test-group "yaml?"
		(test? (not (yaml? (lambda (?) '()))))
		(test? (not (yaml? (lambda (?) '("after zero" 2 #(3 4) (( (() . #f) )))))))
		(test? (not (yaml? (lambda (? . ...)
			'()))))
		(test? (not (yaml? (lambda (? . ...)
			'("after zero" 2 #(3 4) (( (() . #f) )))))))
		(test? (yaml? (yaml<- "")))
		(test? (yaml?
			(with-input-from-file "/dev/null"
				(lambda () (yaml<- (current-input-port))))))
	)
)

(test-group "ss2yaml.ss"
	(test-group "<-yaml"
		(test-error (<-yaml (yaml<- "")))
		(test? (not (not (<-yaml #:strict-input (yaml<- "")))))
		(test? (not (not (<-yaml `()))))
	)
)
