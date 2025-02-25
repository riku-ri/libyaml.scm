(module libyaml ()

(import scheme)
(import (chicken base))
(import (chicken syntax))
(import (chicken foreign))
(import (chicken keyword))
(import (chicken format))
(import (chicken string))
(import (chicken irregex))

(foreign-declare "#include <string.h>")
(foreign-declare "#include <yaml.h>")

(define-syntax foreign-value-index (syntax-rules ()
	((foreign-value-index index symbol value)
		(begin
			(define symbol value)
			(define index
				(if (assoc symbol index)
					index
					(cons (cons symbol (quote symbol)) index)))))))

(define-foreign-type yaml_error_type_t int)
; XXX: Here use int as return value is because the typedef cannot be set in (foreigin-lambda*)
;(define-foreign-type bool int (lambda (?) (if ? 1 0)) (lambda (?) (if (= ? 0) #f #t)))
; foreign-type bool had been defined in chicken
(define >yaml_error_type_e< '())
(foreign-value-index >yaml_error_type_e< YAML_MEMORY_ERROR (foreign-value "YAML_MEMORY_ERROR" yaml_error_type_t))
(foreign-value-index >yaml_error_type_e< YAML_READER_ERROR (foreign-value "YAML_READER_ERROR" yaml_error_type_t))
(foreign-value-index >yaml_error_type_e< YAML_SCANNER_ERROR (foreign-value "YAML_SCANNER_ERROR" yaml_error_type_t))
(foreign-value-index >yaml_error_type_e< YAML_PARSER_ERROR (foreign-value "YAML_PARSER_ERROR" yaml_error_type_t))
(foreign-value-index >yaml_error_type_e< YAML_COMPOSER_ERROR (foreign-value "YAML_COMPOSER_ERROR" yaml_error_type_t))
(foreign-value-index >yaml_error_type_e< YAML_WRITER_ERROR (foreign-value "YAML_WRITER_ERROR" yaml_error_type_t))
(foreign-value-index >yaml_error_type_e< YAML_EMITTER_ERROR (foreign-value "YAML_EMITTER_ERROR" yaml_error_type_t))
(foreign-value-index >yaml_error_type_e< YAML_NO_ERROR (foreign-value "YAML_NO_ERROR" yaml_error_type_t))

(define-foreign-type yaml_encoding_t int)
(define >yaml_encoding_e< '())
(foreign-value-index >yaml_encoding_e< YAML_ANY_ENCODING (foreign-value "YAML_ANY_ENCODING" yaml_encoding_t))
(foreign-value-index >yaml_encoding_e< YAML_UTF8_ENCODING (foreign-value "YAML_UTF8_ENCODING" yaml_encoding_t))
(foreign-value-index >yaml_encoding_e< YAML_UTF16LE_ENCODING (foreign-value "YAML_UTF16LE_ENCODING" yaml_encoding_t))
(foreign-value-index >yaml_encoding_e< YAML_UTF16BE_ENCODING (foreign-value "YAML_UTF16BE_ENCODING" yaml_encoding_t))

(define-foreign-type yaml_event_type_t int)
(define >yaml_event_type_e< '())
(foreign-value-index >yaml_event_type_e< YAML_ERROR_EVENT (foreign-value "(-1)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_NO_EVENT (foreign-value "(YAML_NO_EVENT)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_STREAM_START_EVENT (foreign-value "(YAML_STREAM_START_EVENT)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_STREAM_END_EVENT (foreign-value "(YAML_STREAM_END_EVENT)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_DOCUMENT_START_EVENT (foreign-value "(YAML_DOCUMENT_START_EVENT)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_DOCUMENT_END_EVENT (foreign-value "(YAML_DOCUMENT_END_EVENT)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_ALIAS_EVENT (foreign-value "(YAML_ALIAS_EVENT)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_SCALAR_EVENT (foreign-value "(YAML_SCALAR_EVENT)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_SEQUENCE_START_EVENT (foreign-value "(YAML_SEQUENCE_START_EVENT)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_SEQUENCE_END_EVENT (foreign-value "(YAML_SEQUENCE_END_EVENT)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_MAPPING_START_EVENT (foreign-value "(YAML_MAPPING_START_EVENT)" yaml_event_type_t))
(foreign-value-index >yaml_event_type_e< YAML_MAPPING_END_EVENT (foreign-value "(YAML_MAPPING_END_EVENT)" yaml_event_type_t))

(define yaml_parser_set_encoding (foreign-lambda void "yaml_parser_set_encoding" (c-pointer "yaml_parser_t") yaml_encoding_t))
(define yaml_parser_initialize (foreign-lambda int "yaml_parser_initialize" (c-pointer "yaml_parser_t")))
(define yaml_parser_set_input_file (foreign-lambda void "yaml_parser_set_input_file" (c-pointer "yaml_parser_t") (c-pointer "FILE")))
(define yaml_parser_set_input_string (foreign-lambda void "yaml_parser_set_input_string" (c-pointer "yaml_parser_t") c-string size_t))
(define yaml_parser_parse (foreign-lambda int "yaml_parser_parse" (c-pointer "yaml_parser_t") (c-pointer "yaml_event_t")))
(define yaml_event_delete (foreign-lambda void "yaml_event_delete" (c-pointer "yaml_event_t")))
(define yaml_parser_delete (foreign-lambda void "yaml_parser_delete" (c-pointer "yaml_parser_t")))

(define-syntax write/ (syntax-rules () ((write/ towrite ...) (let () (write towrite ...) (print "")))))

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

(define (libyaml:read . ><)
	(let*
		(
			(>< (argparse >< '() (list #:input #:encoding)))
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
								(cons
									(eval (car <enum>))
									; ERR: when export as module, the symbol is not symbols defined before
									(list (cons #:string (symbol->string (car <enum>)))))
								((? ?) (cdr <enum>)))))))))
			(c-string-or-empty (lambda (tocheck) (let* ((str tocheck)) (if str str ""))))
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
		(let ((port->FILE* (foreign-lambda c-pointer "C_port_file" scheme-object)))
			(if (assoc #:input (cdr ><))
				(let ((input (cdr (assoc #:input (cdr ><)))))
					(cond
						((string? input)
							(yaml_parser_set_input_string (&parser) input (string-length input)))
						((input-port? input) (yaml_parser_set_input_file (&parser) (port->FILE* input)))))
				(yaml_parser_set_input_file (&parser) (port->FILE* (current-input-port)))))

		(if (assoc #:encoding (cdr ><))
			(let ((encoding (cdr (assoc #:encoding (cdr ><)))))
				(yaml_parser_set_encoding (&parser) encoding)))

		(if (assoc #:encoding (cdr ><))
			(if (not
				(=
					(*-> "yaml_parser_t" (&parser) "encoding" yaml_encoding_t)
					(cdr (assoc #:encoding (cdr ><)))))
				(error
					(sprintf "cannot set encoding to ~S" (cdr (assoc #:encoding (cdr ><))))
					(sprintf "enable encodings should be in ~A"
						(map cdr >yaml_encoding_e<)))))
		(define <anchor> (list))
		(define (:libyaml:read event)
			(if
				(or
					(*-> "yaml_event_t" (&event) "data.scalar.tag" c-string)
					(*-> "yaml_event_t" (&event) "data.sequence_start.tag" c-string)
					(*-> "yaml_event_t" (&event) "data.mapping_start.tag" c-string)
				)
				(warning (sprintf "tag at [line:~A , colunm:~A]" ; TODO: make tage to a vector
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
										((irregex-match? "\\.nan|\\.NaN|\\.NAN" <<)
											(string->number (string-append
												"+"
												(list->string (map char-downcase (string->list (substring << 1))))
												".0")))
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
									(let* ((event (yaml-parser-parse (&parser) (&event))))
										(cond
											((= event YAML_MAPPING_END_EVENT) '())
											(else
												(let* ; Use (let*) to make sure value is after key
													(
														(k (:libyaml:read event))
														(v (:libyaml:read (yaml-parser-parse (&parser) (&event))))
													)
													(let ((<< (cons (cons k v) ((@ @))))) <<))))))))))
						)
						(let ((mapping (lambda () mapping)))
							(if anchor (if (not (assoc anchor <anchor>)) (set! <anchor> (cons (cons anchor mapping) <anchor>))))
							mapping))) ; Use (lambda) to distinguish yaml-list and yaml-mapping
			) ; (cond)
		)
		(:libyaml:read (yaml-parser-parse (&parser) (&event)))))

;(define (libyaml:fixed-mapping yaml)
;	(cond
;		((list? yaml) (map libyaml:fixed-mapping yaml))
;		((pair? yaml) (cons (libyaml:fixed-mapping (car yaml)) (libyaml:fixed-mapping (cdr yaml))))
;		(else (if (procedure? yaml) (libyaml:fixed-mapping (yaml)) yaml))))

(define (libyaml:ordered-mapping . yaml><)
	(if (null? yaml><) (error "no yaml provided"))
	(define yaml (car yaml><))
	(define >< (argparse (cdr yaml><) '() '(#:switch)))
	(let
		(
			(switch (if (assoc #:switch (cdr ><))
				(cdr ><)
				(lambda (l r) (string>? (sprintf "~S" (car l)) (sprintf "~S" (car r))))))
		)

		(define (sort <>)
			(define (:sort ^ ... <<)
				(cond
					((null? ...) (cons ^ <<))
					(else
						(if (switch ^ (car ...))
							(:sort (car ...) (cdr ...) (cons ^ <<))
							(:sort ^ (cdr ...) (cons (car ...) <<))))))
			(if (null? <>) <> (:sort (car <>) (cdr <>) '())))
		(define (:libyaml:ordered-mapping yaml)
			(cond
				((null? yaml) '())
				((list? yaml) (map :libyaml:ordered-mapping yaml))
				((pair? yaml) (cons (:libyaml:ordered-mapping (car yaml)) (:libyaml:ordered-mapping (cdr yaml))))
				(else (if (procedure? yaml)
					(let ((yaml (yaml)))
						(:libyaml:ordered-mapping (sort yaml)))
					yaml))))
		(:libyaml:ordered-mapping yaml)
))

(define (libyaml:dump . yaml><)
	(if (null? yaml><) (error "no yaml provided"))
	(let
		(
			(yaml (car yaml><))
			(>< (argparse
				(cdr yaml><)
				'(#:string #:oneline #:1-document-wrap)
				'(#:port #:indent)))
		)
		(define (:libyaml:dump-document yaml)
			(cond
				((null? yaml) "~") ; TODO
				((procedure? yaml) '()) ; TODO
				((list? yaml) '() ; Note that '() is also list
				) ; TODO
				(else (let((yaml (cond
					((string? yaml) (string-split yaml "\n" #t))
					((number? yaml)
						(cond
							((nan? yaml) ".NAN")
							((infinite? yaml) (if (> yaml 0) "+.inf" "-.inf"))
							(else (number->string yaml))))
					((boolean? yaml) (if yaml "true" "false"))
					))) yaml))))
		(append
			(if (or (> (length yaml) 1) (member #:1-document-wrap (car ><))) '("---") '())
			(flatten (join (map list (map :libyaml:dump-document yaml)) '("..." "---")))
			(if (or (> (length yaml) 1) (member #:1-document-wrap (car ><))) '("...") '()))))

(define yaml (libyaml:read))
(write/ yaml)
;(write/
;;(libyaml:dump yaml #:1-document-wrap)
;(libyaml:dump yaml)
;)
(write/ (libyaml:ordered-mapping yaml))

)
