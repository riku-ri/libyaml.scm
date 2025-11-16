(module (libyaml yaml2ss)
	(
		yaml<-
	)

(import scheme)
(import (chicken base))
(import (chicken string))
(import (chicken format))
(import (chicken memory))
(import (chicken irregex))
(import (chicken foreign))
(import (chicken condition))
(import (libyaml yaml.h))
(import varg)

(foreign-declare "#include <yaml.h>")

(define-foreign-type enum int)

(define (yaml<- . <>) (let-syntax
	(
		(*-> (syntax-rules ()
			((*-> type-string pointer to-access ... return-type)
				(
					(foreign-lambda* return-type (((c-pointer type-string) _p))
						"C_return((_p)->" to-access ... ");")
					pointer
				))))
		(abort (syntax-rules ()
			((abort condition ...) (begin
				(print-call-chain (current-error-port))(newline (current-error-port))
				(abort condition ...)))))
		(ycondition (syntax-rules ()
			((ycondition ? ...)
				(condition (list 'libyaml ? ...)))))
	) (let*
	(
		(errtag "[yaml<-]")
		(inerr "internal logic error, please contact maintainer")
		(>< (handle-exceptions e
			(abort (ycondition (apply condition
				(map (lambda (c) (cons 'libyaml (cdr c))) (condition->list e)))))
			; exception kind: 'varg -> 'libyaml
			(varg <> '(#:with-value #:encoding) #:enable-unknown)))
		(input (let ((literal (cdr (assoc #:literal ><))))
			(cond
				((> (length literal) 1) (abort (ycondition
					'message (sprintf "unknown arguments:\n~S" (cdr literal)))))
				((= (length literal) 0) (current-input-port))
				((= (length literal) 1) (car literal)))))
		(string->number (lambda (?)
			(if (string->number ?)
				(string->number ?)
				(abort (ycondition
					'message (sprintf "(string->number) convert error:\n~S" ?))))))
		(memset (foreign-lambda c-pointer "memset" c-pointer int size_t))
		(&parser (allocate (foreign-type-size "struct yaml_parser_s")))
		(&event (allocate (foreign-type-size "struct yaml_event_s")))
		(close (lambda ()
			(yaml_event_delete &event) (yaml_parser_delete &parser)
			(free &parser) (free &event)
			(if (input-port? input) (close-input-port input)))
		)
	) (let-syntax
	(
		(abort (syntax-rules ()
			((abort condition ...) (begin
				(close)
				(abort condition ...)))))
	) (let*
	(
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
										"[~S] ~S ~S at [line:~S , colunm:~S]"
										(*-> "yaml_parser_t" &parser "error" enum)
										(c-string-or-empty (*-> "yaml_parser_t" &parser "problem" c-string))
										(c-string-or-empty (*-> "yaml_parser_t" &parser "context" c-string))
										(+ 1 (*-> "yaml_parser_t" &parser "problem_mark.line" size_t))
										(+ 1 (*-> "yaml_parser_t" &parser "problem_mark.column" size_t))))
							)
							(abort (ycondition 'message errmessage))))
					(else
						(*-> "yaml_event_t" event "type" enum)
					)
				)))
	)
	(memset &parser 0 (foreign-type-size "struct yaml_parser_s"))
	(memset &event 0 (foreign-type-size "struct yaml_event_s"))
	(let*
		(
			(<< (yaml_parser_initialize &parser))
			(err (*-> "yaml_parser_t" &parser "error" enum))
		)
		(cond
			((not (= 1 <<))
				(abort (ycondition 'message (sprintf
					(string-append
						"yaml_parser_initialize() failed and return [~S] but not [1]."
						" "
						"Error code in struct yaml_parser_s is [~S]")
					<<
					err))))))
	(let ((port->FILE* (foreign-lambda c-pointer "C_port_file" scheme-object)))
		(cond
			((string? input)
				(yaml_parser_set_input_string
					&parser
					input
					(string-length input)))
			((input-port? input)
				(yaml_parser_set_input_file &parser (port->FILE* input)))
			(else
				(abort (ycondition 'message (sprintf
					"input is not a string or input-port:\n~S"
					input))))
		))

	(let ((with-value (cdr (assoc #:with-value ><))))
		(cond
			((assoc #:encoding with-value)
				(let ((encoding (cdr (assoc #:encoding with-value))))
					(yaml_parser_set_encoding &parser encoding)
					(cond
						((not (= (*-> "yaml_parser_t" &parser "encoding" enum) encoding))
							(abort (ycondition 'message (sprintf
								"set encoding to ~S failed" encoding)))))))))
	(define <anchor> (list))
	(define (:yaml<- event)
		(cond
			((or
				(*-> "yaml_event_t" &event "data.scalar.tag" c-string)
				(*-> "yaml_event_t" &event "data.sequence_start.tag" c-string)
				(*-> "yaml_event_t" &event "data.mapping_start.tag" c-string)
			)
			(abort (ycondition 'message (sprintf
				"tag at [line:~S , colunm:~S]"
				(+ 1 (*-> "yaml_event_t" &event "start_mark.line" size_t))
				(+ 1 (*-> "yaml_event_t" &event "start_mark.column" size_t)))
				"yaml-tag is not supported"))))
		(cond
			((= event YAML_NO_EVENT)
				(abort (ycondition 'message (sprintf
					"You should never go into this event ~S" 'YAML_NO_EVENT))))
			((= event YAML_ALIAS_EVENT)
				(let
					(
						(anchor (*-> "yaml_event_t" &event "data.alias.anchor" c-string))
					)
					(cond
						((not (assoc anchor <anchor>))
							(abort (ycondition 'message
								"No reference or circular reference to anchor" anchor))))
					(cdr (assoc anchor <anchor>))))
			((= event YAML_SCALAR_EVENT)
				(let*
					(
						(anchor (*-> "yaml_event_t" &event "data.scalar.anchor" c-string))
						(<< (*-> "yaml_event_t" &event "data.scalar.value" c-string))
					)
					(let
						(
							(<< (if (*-> "yaml_event_t" &event "data.scalar.plain_implicit" bool)
							; XXX: implicit yaml-tag value also not set plain_implicit.
							; But yaml-tag will generate an error
								(cond
									; Regular expression is from https://yaml.org/spec/1.2.2/
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
												(list->string
													(map char-downcase (string->list (substring << (if sign 2 1)))))
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
						(if anchor (if (not (assoc anchor <anchor>))
							(set! <anchor> (cons (cons anchor <<) <anchor>))))
						<<)))
			((= event YAML_STREAM_START_EVENT)
				(
					((lambda (@) (@ @)) (lambda (@) (lambda ()
						(let* ((event (yaml-parser-parse &parser &event)))
							(cond
								((= event YAML_STREAM_END_EVENT) '())
								; If yaml-parser-parse is later then check YAML_SEQUENCE_END_EVENT,
								; it will return an undefined value but not '() here
								(else (let ((<< (cons (:yaml<- event) ((@ @))))) <<)))))))
				))
			((= event YAML_DOCUMENT_START_EVENT)
				(
					((lambda (@) (@ @)) (lambda (@) (lambda (last)
						(let* ((event (yaml-parser-parse &parser &event)))
						(cond
							((= event YAML_DOCUMENT_END_EVENT) last)
							(else
								; ((@ @) (:yaml<- event))
								(abort (ycondition 'message
									(string-append
										"YAML_DOCUMENT_END_EVENT does not appear after "
										"twice yaml_parser_parse from YAML_DOCUMENT_START_EVENT"
										"This may be a bug in libyaml itself,"
										"it was supposed to generate a parser error here"
								)))))))))
					(let* ((<< (:yaml<- (yaml-parser-parse &parser &event)))) <<)
				))
			((= event YAML_SEQUENCE_START_EVENT)
				(let
					(
						(yaml
							(
								((lambda (@) (@ @)) (lambda (@) (lambda ()
									(let* ; anchor need to be cached before next parse
										(
											(anchor (*-> "yaml_event_t" &event "data.sequence_start.anchor" c-string))
											(event (yaml-parser-parse &parser &event))
										)
										; If not get event but use the value in member of &event
										; all recursion will end by the most internal YAML_SEQUENCE_END_EVENT
										; this error will not occure in YAML_STREAM_START_EVENT
										; because stream would never be nested
										(cond
											((= event YAML_SEQUENCE_END_EVENT) '())
											(else
												(let ((<< (cons (:yaml<- event) ((@ @)))))
													(if anchor (if (not (assoc anchor <anchor>))
														(set! <anchor> (cons (cons anchor <<) <anchor>))))
													<<)
											))))))
							))
					)
					(list->vector yaml)
					)
			)
			((= event YAML_MAPPING_START_EVENT)
				(let*
					(
						(anchor
							(*-> "yaml_event_t" &event "data.mapping_start.anchor" c-string))
						(mapping (
							((lambda (@) (@ @)) (lambda (@) (lambda ()
								(let* ((event (yaml-parser-parse &parser &event)))
									(cond
										((= event YAML_MAPPING_END_EVENT) '())
										(else
											(let* ; Use (let*) to make sure value is after key
												(
													(k (:yaml<- event))
													(v (:yaml<- (yaml-parser-parse &parser &event)))
												)
												(let ((<< (cons (cons k v) ((@ @))))) <<))))))))))
					)
					(if anchor
						(if (not (assoc anchor <anchor>))
							(set! <anchor> (cons (cons anchor mapping) <anchor>))))
					(list mapping)))
		) ; (cond)
	)
	(let*
		(
			(document-list (:yaml<- (yaml-parser-parse &parser &event)))
			(document-list (if (null? document-list) '(()) document-list))
			; empty file will not go into SCALAR event and get ~
		)
		(define (yaml . document-index)
			(cond
				((null? document-index) (list-ref document-list 0))
				((> (length document-index) 1)
					(abort (ycondition 'message (sprintf
						"unknown arguments:\n~S" (cdr document-index)))))
				((= (length document-index) 1)
					(let ((i (car document-index)))
						(if (not (integer? i))
							(abort (ycondition 'message (sprintf
								"document index is not a integer ~S" i))))
						(cond
							((>= i (length document-list))
								(abort (ycondition 'message (sprintf
									"index ~S is lager then max document index ~S"
									i (- (length document-list) 1)))))
							((< i -1) (abort (ycondition 'message (sprintf
								"invalid document index ~S" i))))
							((= i -1) document-list)
							(else (list-ref document-list i)))
					)
				)
				(else (abort (ycondition 'messageerrtag inerr)))
			))
		(close)
		yaml
	)
))))) ;define/let/let-syntax

) ;module
