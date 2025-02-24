(import scheme)
(import (chicken foreign))
(import (chicken keyword))
(import (chicken format))
(import (chicken irregex))

(foreign-declare "#include <string.h>")
(foreign-declare "#include <yaml.h>")

(define-foreign-type yaml_error_type_t int)
; XXX: Here use int as return value is because the typedef cannot be set in (foreigin-lambda*)

;(define-foreign-type bool int (lambda (?) (if ? 1 0)) (lambda (?) (if (= ? 0) #f #t)))
; foreign-type bool had been defined in chicken

(define NULL (foreign-value "NULL" c-pointer))
(define YAML_NO_ERROR (foreign-value "YAML_NO_ERROR" yaml_error_type_t))
(define YAML_MEMORY_ERROR (foreign-value "YAML_MEMORY_ERROR" yaml_error_type_t))
(define YAML_READER_ERROR (foreign-value "YAML_READER_ERROR" yaml_error_type_t))
(define YAML_SCANNER_ERROR (foreign-value "YAML_SCANNER_ERROR" yaml_error_type_t))
(define YAML_PARSER_ERROR (foreign-value "YAML_PARSER_ERROR" yaml_error_type_t))
(define YAML_COMPOSER_ERROR (foreign-value "YAML_COMPOSER_ERROR" yaml_error_type_t))
(define YAML_WRITER_ERROR (foreign-value "YAML_WRITER_ERROR" yaml_error_type_t))
(define YAML_EMITTER_ERROR (foreign-value "YAML_EMITTER_ERROR" yaml_error_type_t))

(define-foreign-type yaml_encoding_t int)
(define YAML_ANY_ENCODING (foreign-value "YAML_ANY_ENCODING" yaml_encoding_t))
(define YAML_UTF8_ENCODING (foreign-value "YAML_UTF8_ENCODING" yaml_encoding_t))
(define YAML_UTF16LE_ENCODING (foreign-value "YAML_UTF16LE_ENCODING" yaml_encoding_t))
(define YAML_UTF16BE_ENCODING (foreign-value "YAML_UTF16BE_ENCODING" yaml_encoding_t))

(define-foreign-type yaml_event_type_t int)
(define YAML_ERROR_EVENT (foreign-value "(-1)" yaml_event_type_t))
(define YAML_NO_EVENT (foreign-value "(YAML_NO_EVENT)" yaml_event_type_t))
(define YAML_STREAM_START_EVENT (foreign-value "(YAML_STREAM_START_EVENT)" yaml_event_type_t))
(define YAML_STREAM_END_EVENT (foreign-value "(YAML_STREAM_END_EVENT)" yaml_event_type_t))
(define YAML_DOCUMENT_START_EVENT (foreign-value "(YAML_DOCUMENT_START_EVENT)" yaml_event_type_t))
(define YAML_DOCUMENT_END_EVENT (foreign-value "(YAML_DOCUMENT_END_EVENT)" yaml_event_type_t))
(define YAML_ALIAS_EVENT (foreign-value "(YAML_ALIAS_EVENT)" yaml_event_type_t))
(define YAML_SCALAR_EVENT (foreign-value "(YAML_SCALAR_EVENT)" yaml_event_type_t))
(define YAML_SEQUENCE_START_EVENT (foreign-value "(YAML_SEQUENCE_START_EVENT)" yaml_event_type_t))
(define YAML_SEQUENCE_END_EVENT (foreign-value "(YAML_SEQUENCE_END_EVENT)" yaml_event_type_t))
(define YAML_MAPPING_START_EVENT (foreign-value "(YAML_MAPPING_START_EVENT)" yaml_event_type_t))
(define YAML_MAPPING_END_EVENT (foreign-value "(YAML_MAPPING_END_EVENT)" yaml_event_type_t))

(define-foreign-type yaml_scalar_style_t int)

(define yaml_parser_set_encoding (foreign-lambda void "yaml_parser_set_encoding" (c-pointer "yaml_parser_t") yaml_encoding_t))
(define yaml_parser_initialize (foreign-lambda int "yaml_parser_initialize" (c-pointer "yaml_parser_t")))
(define yaml_parser_set_input_file (foreign-lambda void "yaml_parser_set_input_file" (c-pointer "yaml_parser_t") (c-pointer "FILE")))
(define yaml_parser_set_input_string (foreign-lambda void "yaml_parser_set_input_string" (c-pointer "yaml_parser_t") c-string size_t))
(define yaml_parser_parse (foreign-lambda int "yaml_parser_parse" (c-pointer "yaml_parser_t") (c-pointer "yaml_event_t")))
(define yaml_event_delete (foreign-lambda void "yaml_event_delete" (c-pointer "yaml_event_t")))
(define yaml_parser_delete (foreign-lambda void "yaml_parser_delete" (c-pointer "yaml_parser_t")))
(define yaml-event->scalar.value (foreign-lambda* (c-pointer "yaml_char_t") (((c-pointer "yaml_event_t") yaml_event_p)) "C_return((yaml_event_p)->data.scalar.value);"))

(define (argparse <> <without-value> <with-value>)
	(if (not (list? <>)) (error "argument is not a list" <>))
	(define (:argparse <> <arg> >arg<)
		(cond
			((null? <>) (cons <arg> >arg<))
			((keyword? (car <>))
				(if (not (member (car <>) <without-value>))
					(error (sprintf "without-value option not in ~S" <without-value>) (car <>)))
				(:argparse (cdr <>) (cons (car <>) <arg>) >arg<))
			((pair? (car <>))
				(if (not (member (car (car <>)) <with-value>))
					(error (sprintf "with-value option is not in ~S" <with-value>) (car (car <>))))
				(:argparse (cdr <>) <arg> (cons (car <>) >arg<)))
			(else (error "argument unit is not keyword or key-value pair" (car <>)))))
	(:argparse <> '() '()))

(define-syntax *-> (syntax-rules () ((*-> type-string pointer to-access ... return-type) ((foreign-lambda* return-type (((c-pointer type-string) _p)) "C_return((_p)->" to-access ... ");") pointer))))
(define (libyaml:read . >argparse<) ; TODO anchor
	(define >< (argparse >argparse< '() (list #:input #:encoding)))
	(let*
		(
			(string->number (lambda (?) (if (string->number ?) (string->number ?) (error "(string->number) convert error" ?))))
			(memset (foreign-lambda c-pointer "memset" c-pointer int size_t))
			(&parser (foreign-lambda* (c-pointer "yaml_parser_t") () "static yaml_parser_t yaml_parser; C_return(&yaml_parser);"))
			(&event (foreign-lambda* (c-pointer "yaml_event_t") () "static yaml_event_t yaml_event; C_return(&yaml_event);"))
			(clear (lambda () (close-input-port (current-input-port)) (yaml_event_delete (&event)) (yaml_parser_delete (&parser))))
			(@enum ; construct an association list that contain (#:string . (symbol->string))
				((lambda (@) (@ @))
					(lambda (?) (lambda (<enum>)
						(cond
							((null? <enum>) '())
							(else (cons `(,(eval (car <enum>)) (#:string . ,(symbol->string (car <enum>)))) ((? ?) (cdr <enum>)))))))))
			(c-string-or-empty (lambda (tocheck) (let* ((str tocheck)) (if str str ""))))
			(>yaml_error_type_e< (@enum '(
				YAML_NO_ERROR
				YAML_MEMORY_ERROR
				YAML_READER_ERROR
				YAML_SCANNER_ERROR
				YAML_PARSER_ERROR
				YAML_COMPOSER_ERROR
				YAML_WRITER_ERROR
				YAML_EMITTER_ERROR
			)))
			(>yaml_event_type_e< (@enum '(
				YAML_NO_EVENT
				YAML_STREAM_START_EVENT
				YAML_STREAM_END_EVENT
				YAML_DOCUMENT_START_EVENT
				YAML_DOCUMENT_END_EVENT
				YAML_ALIAS_EVENT
				YAML_SCALAR_EVENT
				YAML_SEQUENCE_START_EVENT
				YAML_SEQUENCE_END_EVENT
				YAML_MAPPING_START_EVENT
				YAML_MAPPING_END_EVENT
			)))
			(>yaml_encoding_e< (@enum '(
				YAML_ANY_ENCODING
				YAML_UTF8_ENCODING
				YAML_UTF16LE_ENCODING
				YAML_UTF16BE_ENCODING
			)))
			(yaml-parser-parse
				(lambda (parser event)
					(cond
						((not (= (yaml_parser_parse parser event) 1)) ; According to comment in yaml.h , yaml_parser_parse() return 1 if the function succeeded
							(let*
								(
									(errmessage (sprintf
										"[~A] ~A ~A at [line:~A , colunm:~A]"
										(*-> "yaml_parser_t" (&parser) "error" yaml_error_type_t)
										(c-string-or-empty (*-> "yaml_parser_t" (&parser) "problem" c-string))
										(c-string-or-empty (*-> "yaml_parser_t" (&parser) "context" c-string))
										(+ 1 (*-> "yaml_parser_t" (&parser) "problem_mark.line" size_t))
										(+ 1 (*-> "yaml_parser_t" (&parser) "problem_mark.column" size_t))))
								)
							(clear)
							(error errmessage)
							YAML_ERROR_EVENT))
						(else
							;(print (cdr (assoc (*-> "yaml_event_t" event "type" yaml_event_type_t) >yaml_event_type_e<)))
							(*-> "yaml_event_t" event "type" yaml_event_type_t)
						)
					)))
		)
		(memset (&parser) 0 (foreign-type-size "yaml_parser_t"))
		(memset (&event) 0 (foreign-type-size "yaml_event_t"))

		(let* ((<< (yaml_parser_initialize (&parser)))) (if (not (= 1 <<))
		; According to comment in <yaml.h>, yaml_parser_initialize() return 1 when succeeded
			(error (sprintf "[~A] with error [~A]"
				<<
				(*-> "yaml_parser_t" (&parser) "error" yaml_error_type_t)))))
		(if (assoc #:input (cdr ><))
			(let ((input (cdr (assoc #:input (cdr ><)))))
				(cond
					((string? input) (yaml_parser_set_input_string (&parser) input (string-length input)))
					((input-port? input) (yaml_parser_set_input_file (&parser) input))))
			(yaml_parser_set_input_file (&parser) (current-input-port)))
		(yaml_parser_set_encoding (&parser) (if (assoc #:encoding (cdr ><)) (cdr (assoc #:encoding (cdr ><))) YAML_ANY_ENCODING))
		(if (assoc #:encoding (cdr ><))
			(if (not (=
				((foreign-lambda* yaml_encoding_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->encoding);") (&parser))
				(cdr (assoc #:encoding (cdr ><)))))
				(error (sprintf "~S is not in ~A" #:encoding (map (lambda (?) (cdr (assoc #:string (cdr ?)))) >yaml_encoding_e<)))))
		(set! <anchor> (list))
		(define (:libyaml:read event)
			(cond
				((= (*-> "yaml_event_t" (&event) "type" yaml_event_type_t) YAML_NO_EVENT) (error (sprintf "You should never go into this event ~S" 'YAML_NO_EVENT)))
				((= event YAML_SCALAR_EVENT)
					(let* ((<< (*-> "yaml_event_t" (&event) "data.scalar.value" c-string)))
						(if (*-> "yaml_event_t" (&event) "data.scalar.plain_implicit" bool)
						; XXX: implicit yaml-tag value also not set plain_implicit. But yaml-tag will be ignored and always return the literal value
							(cond
								((or (= (string-length <<) 0) (irregex-match? "null|Null|NULL|~" <<)) '())
								((irregex-match? "true|True|TRUE|false|False|FALSE" <<) (let* ((^ (char-downcase (string-ref << 0))))
									(cond ((char=? ^ #\f) #f) ((char=? ^ #\f) #f) ((char=? ^ #\t) #t))))
								((or (irregex-match? "[-+]?[0-9]+" <<) (irregex-match? "[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?" <<)) (string->number <<))
								((or (irregex-match? "0o[0-7]+" <<) (irregex-match? "0x[0-9a-fA-F]+" <<)) (string->number (string-append "#" (substring << 1))))
								((irregex-match? "[-+]?(\\.inf|\\.Inf|\\.INF)" <<)
									(let ((sign (not (char=? #\. (string-ref << 0)))))
										(string->number (string-append
											(if sign (substring << 0 1) "+")
											(list->string (map char-downcase (string->list (substring << (if sign 2 1)))))
											".0"))))
								((irregex-match? "\\.nan|\\.NaN|\\.NAN" <<) (string-append
									"+"
									(list->string (map char-downcase (string->list (substring << 1))))
									".0"))
								(else <<)
							)
							<<)))
				((= event YAML_STREAM_START_EVENT)
					(
						((lambda (@) (@ @)) (lambda (@) (lambda ()
							(let* ((event (yaml-parser-parse (&parser) (&event))))
								(cond
									((= event YAML_STREAM_END_EVENT) '())
									; If yaml-parser-parse is later then check YAML_SEQUENCE_END_EVENT,
									; it will return an undefined value but not '() here
									(else
										(let* ((<< (cons (:libyaml:read event) ((@ @))))) <<)
									))))))
					))
				((= event YAML_DOCUMENT_START_EVENT)
					(
						((lambda (@) (@ @)) (lambda (@) (lambda (last)
							(let* ((event (yaml-parser-parse (&parser) (&event))))
							(cond
								((= event YAML_DOCUMENT_END_EVENT) last)
								(else
									; ((@ @) (:libyaml:read event))
									(error
										(cdr (assoc #:string (cdr (assoc event >yaml_event_type_e<))))
										"YAML_DOCUMENT_END_EVENT does not appear after twice yaml_parser_parse from YAML_DOCUMENT_START_EVENT"
										"This may be a bug in libyaml itself, it was supposed to generate a parser error here"
									)
								))))))
						(let* ((<< (:libyaml:read (yaml-parser-parse (&parser) (&event))))) <<)
					))
				((= event YAML_SEQUENCE_START_EVENT)
					(
						((lambda (@) (@ @)) (lambda (@) (lambda ()
							(let* ((event (yaml-parser-parse (&parser) (&event))))
								; If not get event but use the value in member of (&event), all recursion will end by the most internal YAML_SEQUENCE_END_EVENT
								; YAML_STREAM_START_EVENT has no this problem because stream would never be nested
								(cond
									((= event YAML_SEQUENCE_END_EVENT) '())
									(else
										(let* ((<< (cons (:libyaml:read event) ((@ @))))) <<)
									))))))
					))
				((= event YAML_MAPPING_START_EVENT)
					(let* ((mapping
						(
							((lambda (@) (@ @)) (lambda (@) (lambda ()
								(let* ((event (yaml-parser-parse (&parser) (&event))))
									(cond
										((= event YAML_MAPPING_END_EVENT) '())
										(else
											(let* ; Use (let*) to make sure value is after key
												(
													(k (:libyaml:read event))
													(v (:libyaml:read (yaml-parser-parse (&parser) (&event))))
												)
												(let* ((<< (cons (cons k v) ((@ @))))) <<)
										)))))))
						))) (lambda () mapping))) ; Use (lambda) to distinguish yaml-list and yaml-mapping
			) ; (cond)
		)
		(:libyaml:read (yaml-parser-parse (&parser) (&event)))))

(define (libyaml:dump . >yaml-with-argparse<)
	(if (null? >yaml-with-argparse<) (error "no yaml provided"))
	(define yaml (car >yaml-with-argparse<))
	(define >< (argparse (cdr >yaml-with-argparse<) '(#:string #:oneline) '(#:port #:indent)))
	(current-output-port (if (assoc #:port (cdr ><)) (cdr (assoc #:port (cdr ><))) (current-output-port)))
	(define indent
		(if (assoc #:indent (cdr ><))
			(let ((indent (cdr (assoc #:indent (cdr ><)))))
				(cond
					((integer? indent) (make-string indent #\space))
					((string? indent) indent)
					(else (error "indent is not indent size or indent string" indent))))
			(make-string 2 #\space)))
	(define (<libyaml:dump-document> yaml indent-level)
		(define :indent (((lambda (@) (@ @)) (lambda (@) (lambda (n) (if (= 0 n) "" (string-append indent ((@ @) (- n 1))))))) indent-level))
		(cond
			((procedure? yaml) (let* ; ERROR here mapping is not key-value pair but list of it
				(
					(mapping (yaml))
					(k (<libyaml:dump-document> (car mapping) indent-level))
					(v (<libyaml:dump-document> (cdr mapping) (+ indent-level 1)))
				)
				(if (member #:oneline (car ><))
					(sprintf "{~A: ~A}" k v)
					(sprintf "~A:\n~A" k v))))
			((list? yaml) (let ((l (map (lambda (?) (<libyaml:dump-document> ? (+ indent-level 1))) yaml)))
				(if (member #:oneline (car ><))
					(string-append "[" (foldl string-append "" (join (map list l) (list ","))) "]")
					(eval (cons string-append (join (map list (map (lambda (?) (string-append :indent "- " ?)) l)) '("\n")))))))
			(else (sprintf "~S" yaml)))) ; TODO: multiline string
	(eval (cons string-append (join (map list (map (lambda (?) (string-append "---" "\n" ? "\n" "...")) (map (lambda (?) (<libyaml:dump-document> ? 0)) yaml))) '("\n")))))
(define yaml (libyaml:read))
(write yaml)
;(print (libyaml:dump yaml))

;(set! yaml (libyaml:read))
;(map print
;(list
;(list-ref (car yaml) 0)
;(car (car ((list-ref (car yaml) 1))))
;))
