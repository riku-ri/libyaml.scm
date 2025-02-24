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

(define yaml_parser_set_encoding (foreign-lambda void "yaml_parser_set_encoding" (c-pointer "yaml_parser_t") yaml_encoding_t))
(define yaml_parser_initialize (foreign-lambda int "yaml_parser_initialize" (c-pointer "yaml_parser_t")))
(define yaml_parser_set_input_file (foreign-lambda void "yaml_parser_set_input_file" (c-pointer "yaml_parser_t") (c-pointer "FILE")))
(define yaml_parser_set_input_string (foreign-lambda void "yaml_parser_set_input_string" (c-pointer "yaml_parser_t") c-string size_t))
(define yaml_parser_parse (foreign-lambda int "yaml_parser_parse" (c-pointer "yaml_parser_t") (c-pointer "yaml_event_t")))
(define yaml_event_delete (foreign-lambda void "yaml_event_delete" (c-pointer "yaml_event_t")))
(define yaml_parser_delete (foreign-lambda void "yaml_parser_delete" (c-pointer "yaml_parser_t")))

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

(define-syntax *->
	(syntax-rules ()
		((*-> type-string pointer to-access ... return-type)
			(
				(foreign-lambda* return-type (((c-pointer type-string) _p))
					"C_return((_p)->" to-access ... ");")
				pointer
			))))
(define (libyaml:fixed-mapping yaml)
	(cond
		((list? yaml) (map libyaml:fixed-mapping yaml))
		((pair? yaml) (cons (libyaml:fixed-mapping (car yaml)) (libyaml:fixed-mapping (cdr yaml))))
		(else (if (procedure? yaml) (libyaml:fixed-mapping (yaml)) yaml))))
(define (libyaml:read . ><) ; TODO anchor
	(set! >< (argparse >< '() (list #:input #:encoding)))
	(let*
		(
			(string->number (lambda (?)
				(if (string->number ?)
					(string->number ?)
					(error "(string->number) convert error" ?))))
			(memset (foreign-lambda c-pointer "memset" c-pointer int size_t))
			(&parser (foreign-lambda* (c-pointer "yaml_parser_t") ()
				"static yaml_parser_t yaml_parser; C_return(&yaml_parser);"))
			(&event (foreign-lambda* (c-pointer "yaml_event_t") ()
				"static yaml_event_t yaml_event; C_return(&yaml_event);"))
			(clear (lambda ()
				(close-input-port (current-input-port))
				(yaml_event_delete (&event)) (yaml_parser_delete (&parser))))
			(@enum ; construct an association list that contain (#:string . (symbol->string))
				((lambda (@) (@ @))
					(lambda (?) (lambda (<enum>)
						(cond
							((null? <enum>) '())
							(else (cons
								`(,(eval (car <enum>)) (#:string . ,(symbol->string (car <enum>))))
								((? ?) (cdr <enum>)))))))))
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
						((not (= (yaml_parser_parse parser event) 1))
						; According to comment in yaml.h , yaml_parser_parse() return 1 if the function succeeded
							(let*
								(
									(errmessage
										(sprintf
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
					((string? input)
						(yaml_parser_set_input_string (&parser) input (string-length input)))
					((input-port? input) (yaml_parser_set_input_file (&parser) input))))
			(yaml_parser_set_input_file (&parser) (current-input-port)))
		(yaml_parser_set_encoding
			(&parser)
			(if (assoc #:encoding (cdr ><))
				(cdr (assoc #:encoding (cdr ><)))
				YAML_ANY_ENCODING))
		(if (assoc #:encoding (cdr ><))
			(if (not (= (
					(foreign-lambda* yaml_encoding_t (((c-pointer "yaml_parser_t") _p))
						"C_return((_p)->encoding);")
					(&parser))
				(cdr (assoc #:encoding (cdr ><)))))
				(error (sprintf "~S is not in ~A"
					#:encoding
					(map
						(lambda (?) (cdr (assoc #:string (cdr ?))))
						>yaml_encoding_e<)))))
		(set! <anchor> (list))
		(define (:libyaml:read event)
			(if
				(or
					(*-> "yaml_event_t" (&event) "data.scalar.tag" c-string)
					(*-> "yaml_event_t" (&event) "data.sequence_start.tag" c-string)
					(*-> "yaml_event_t" (&event) "data.mapping_start.tag" c-string)
				)
				(warning (sprintf "tag at [line:~A , colunm:~A]"
					(+ 1 (*-> "yaml_event_t" (&event) "start_mark.line" size_t))
					(+ 1 (*-> "yaml_event_t" (&event) "start_mark.column" size_t)))
					"all tags tag will be ignored and always return the literal value"))
			(cond
				((= event YAML_NO_EVENT)
					(error (sprintf "You should never go into this event ~S"
						'YAML_NO_EVENT)))
				((= event YAML_ALIAS_EVENT)
					(let
						(
							(anchor (*-> "yaml_event_t" (&event) "data.alias.anchor" c-string))
						)
						(if (not (assoc anchor <anchor>))
							(error "No reference or circular reference to anchor" anchor))
						(cdr (assoc anchor <anchor>))))
				((= event YAML_SCALAR_EVENT)
					(let*
						(
							(anchor (*-> "yaml_event_t" (&event) "data.scalar.anchor" c-string))
							(<< (*-> "yaml_event_t" (&event) "data.scalar.value" c-string))
						)
						(let
							(
								(<< (if (*-> "yaml_event_t" (&event) "data.scalar.plain_implicit" bool)
								; XXX: implicit yaml-tag value also not set plain_implicit. But yaml-tag will be ignored and always return the literal value
									(cond
										((or (= (string-length <<) 0) (irregex-match? "null|Null|NULL|~" <<)) '())
										((irregex-match? "true|True|TRUE|false|False|FALSE" <<)
											(let* ((^ (char-downcase (string-ref << 0))))
												(cond ((char=? ^ #\f) #f) ((char=? ^ #\f) #f) ((char=? ^ #\t) #t))))
										(
											(or
												(irregex-match? "[-+]?[0-9]+" <<)
												(irregex-match? "[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?" <<)
											) (string->number <<))
										(
											(or
												(irregex-match? "0o[0-7]+" <<)
												(irregex-match? "0x[0-9a-fA-F]+" <<)
											) (string->number (string-append "#" (substring << 1))))
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
									<<))
							)
							(if anchor (if (not (assoc anchor <anchor>)) (set! <anchor> (cons (cons anchor <<) <anchor>))))
							<<)))
				((= event YAML_STREAM_START_EVENT)
					(
						((lambda (@) (@ @)) (lambda (@) (lambda ()
							(let* ((event (yaml-parser-parse (&parser) (&event))))
								(cond
									((= event YAML_STREAM_END_EVENT) '())
									; If yaml-parser-parse is later then check YAML_SEQUENCE_END_EVENT,
									; it will return an undefined value but not '() here
									(else (let ((<< (cons (:libyaml:read event) ((@ @))))) <<)))))))
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
									)))))))
						(let* ((<< (:libyaml:read (yaml-parser-parse (&parser) (&event))))) <<)
					))
				((= event YAML_SEQUENCE_START_EVENT)
					(
						((lambda (@) (@ @)) (lambda (@) (lambda ()
							(let* ; anchor need to be cached before next parse
								(
									(anchor (*-> "yaml_event_t" (&event) "data.sequence_start.anchor" c-string))
									(event (yaml-parser-parse (&parser) (&event)))
								)
								; If not get event but use the value in member of (&event), all recursion will end by the most internal YAML_SEQUENCE_END_EVENT
								; YAML_STREAM_START_EVENT has no this problem because stream would never be nested
								(cond
									((= event YAML_SEQUENCE_END_EVENT) '())
									(else
										(let ((<< (cons (:libyaml:read event) ((@ @)))))
											(if anchor (if (not (assoc anchor <anchor>)) (set! <anchor> (cons (cons anchor <<) <anchor>))))
											<<)
									))))))
					))
				((= event YAML_MAPPING_START_EVENT)
					(let*
						(
							(anchor (*-> "yaml_event_t" (&event) "data.mapping_start.anchor" c-string))
							(mapping (
								((lambda (@) (@ @)) (lambda (@) (lambda ()
									(let*
										(
											(event (yaml-parser-parse (&parser) (&event)))
										)
										(cond
											((= event YAML_MAPPING_END_EVENT) '())
											(else
												(let* ; Use (let*) to make sure value is after key
													(
														(k (:libyaml:read event))
														(v (:libyaml:read (yaml-parser-parse (&parser) (&event))))
													)
													(let ((<< (cons (cons k v) ((@ @))))) <<)
											)))))))))
						)
						(if anchor (if (not (assoc anchor <anchor>)) (set! <anchor> (cons (cons anchor (lambda () mapping)) <anchor>))))
						(lambda () mapping))) ; Use (lambda) to distinguish yaml-list and yaml-mapping
			) ; (cond)
		)
		(:libyaml:read (yaml-parser-parse (&parser) (&event)))))

(define (libyaml:dump . >^<)
	(if (null? >^<) (error "no yaml provided"))
	(set! ^ (car >^<))
	(set! >< (argparse (cdr >^<) '(#:string #:oneline) '(#:port #:indent)))
	(define (:libyaml:dump-document yaml)
		(cond
			((procedure? yaml) '())
			((list yaml) '())
			(else '())
		)))
(set! yaml (libyaml:fixed-mapping (libyaml:read)))
;(write yaml)
(libyaml:dump yaml)
