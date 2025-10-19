(module (libyaml) *

(import scheme)
(import (chicken base))
(import (chicken syntax))
(import (chicken foreign))
(import (chicken keyword))
(import (chicken format))
(import (chicken string))
(import (chicken irregex))
(import (chicken memory))

(include "foreign.scm")

(foreign-declare "#include <yaml.h>")

(define-foreign-type enum int)

;; FOR test
;(define-syntax write/ (syntax-rules () ((write/ towrite ...) (let () (write towrite ...) (print "")))))

(define (libyaml-argparse <> <without-value> <with-value> before-error)
	(if (not (list? <>)) (error "argument is not a list" <>))
	(define (:libyaml-argparse <> <arg> >arg<)
		(let
			(
				(error (lambda (^ . ...)
					(if (procedure? before-error) (before-error) (apply error (cons ^ ...)))))
			)
			(cond
				((null? <>) (cons <arg> >arg<))
				((keyword? (car <>))
					(if (not (member (car <>) <without-value>))
						(error (sprintf "without-value option not in ~S" <without-value>) (car <>)))
					(:libyaml-argparse (cdr <>) (cons (car <>) <arg>) >arg<))
				((pair? (car <>))
					(if (not (member (car (car <>)) <with-value>))
						(error (sprintf "with-value option is not in ~S" <with-value>) (car (car <>))))
					(:libyaml-argparse (cdr <>) <arg> (cons (car <>) >arg<)))
				(else (error "argument unit is not keyword or key-value pair" (car <>))))))
	(:libyaml-argparse <> '() '()))

(define (yaml<- . ><)
	(define-syntax *->
		(syntax-rules ()
			((*-> type-string pointer to-access ... return-type)
				(
					(foreign-lambda* return-type (((c-pointer type-string) _p))
						"C_return((_p)->" to-access ... ");")
					pointer
				))))
	(let*
		(
			(>< (libyaml-argparse >< '() (list #:input #:encoding) #:no-clear))
			(?input (assoc #:input (cdr ><)))
			(?encoding (assoc #:encoding (cdr ><)))
			(string->number (lambda (?)
				(if (string->number ?)
					(string->number ?)
					(error "(string->number) convert error" ?))))
			(memset (foreign-lambda c-pointer "memset" c-pointer int size_t))
			(&parser (allocate (foreign-type-size "struct yaml_parser_s")))
			(&event (allocate (foreign-type-size "struct yaml_event_s")))
			(clear (lambda ()
				(yaml_event_delete &event)
				(yaml_parser_delete &parser)
				(free &parser)
				(free &event)
				(if ?input
					(let ((input (cdr ?input)))
						(if (input-port? input) (close-input-port input))))))
			(error (lambda (^ . ...) (clear) (apply error (cons ^ ...))))
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
											(*-> "yaml_parser_t" &parser "error" enum)
											(c-string-or-empty (*-> "yaml_parser_t" &parser "problem" c-string))
											(c-string-or-empty (*-> "yaml_parser_t" &parser "context" c-string))
											(+ 1 (*-> "yaml_parser_t" &parser "problem_mark.line" size_t))
											(+ 1 (*-> "yaml_parser_t" &parser "problem_mark.column" size_t))))
								)
								(error errmessage)))
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
					(error (sprintf
						(string-append
							"yaml_parser_initialize() failed and return [~A] but not [1]."
							" "
							"Error code in struct yaml_parser_s is [~A]")
						<<
						err)))))
		(let ((port->FILE* (foreign-lambda c-pointer "C_port_file" scheme-object)))
			(if ?input
				(let ((input (cdr ?input)))
					(cond
						((string? input)
							(yaml_parser_set_input_string
								&parser
								input
								(string-length input)))
						((input-port? input)
							(yaml_parser_set_input_file &parser (port->FILE* input)))
						(else
							(error
								(string-append
									"#:input is not a string or input-port"
									", "
									"also check if the value to #:input is scheme quoted"
								)
							input))
					))
				(yaml_parser_set_input_file &parser (port->FILE* (current-input-port)))))

		(cond
			(?encoding
				(let ((encoding (cdr ?encoding)))
					(yaml_parser_set_encoding &parser encoding))
				(cond
					((not (= (*-> "yaml_parser_t" &parser "encoding" enum) (cdr ?encoding)))
						(error (sprintf "set encoding to ~S failed" (cdr ?encoding)))))))
		(define <anchor> (list))
		(define (:yaml<- event)
			(cond
				((or
					(*-> "yaml_event_t" &event "data.scalar.tag" c-string)
					(*-> "yaml_event_t" &event "data.sequence_start.tag" c-string)
					(*-> "yaml_event_t" &event "data.mapping_start.tag" c-string)
				)
				(error (sprintf "tag at [line:~A , colunm:~A]" ; TODO: make tage to a vector
					(+ 1 (*-> "yaml_event_t" &event "start_mark.line" size_t))
					(+ 1 (*-> "yaml_event_t" &event "start_mark.column" size_t)))
					"yaml-tag is not supported")))
			(cond
				((= event YAML_NO_EVENT)
					(error (sprintf "You should never go into this event ~S" 'YAML_NO_EVENT)))
				((= event YAML_ALIAS_EVENT)
					(let
						(
							(anchor (*-> "yaml_event_t" &event "data.alias.anchor" c-string))
						)
						(cond
							((not (assoc anchor <anchor>))
								(error "No reference or circular reference to anchor" anchor)))
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
										((or (= (string-length <<) 0) (irregex-match? "null|Null|NULL|~" <<)) #())
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
									(error
										(string-append
											"YAML_DOCUMENT_END_EVENT does not appear after "
											"twice yaml_parser_parse from YAML_DOCUMENT_START_EVENT"
											"This may be a bug in libyaml itself,"
											"it was supposed to generate a parser error here"
									))))))))
						(let* ((<< (:yaml<- (yaml-parser-parse &parser &event)))) <<)
					))
				((= event YAML_SEQUENCE_START_EVENT)
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
				((= event YAML_MAPPING_START_EVENT)
					(let*
						(
							(anchor (*-> "yaml_event_t" &event "data.mapping_start.anchor" c-string))
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
						(let ((mapping (lambda () mapping)))
							(if anchor
								(if (not (assoc anchor <anchor>))
								(set! <anchor> (cons (cons anchor mapping) <anchor>))))
							mapping))) ; Use (lambda) to distinguish yaml-list and yaml-mapping
			) ; (cond)
		)
		(let ((yaml (:yaml<- (yaml-parser-parse &parser &event)))) (list->vector yaml))
	) ; let
)

;(define (fixed-mapping yaml)
;	(cond
;		((list? yaml) (map fixed-mapping yaml))
;		((pair? yaml) (cons (fixed-mapping (car yaml)) (fixed-mapping (cdr yaml))))
;		(else (if (procedure? yaml) (fixed-mapping (yaml)) yaml))))

(define (mapping-ordered-yaml<- . yaml><)
	(if (null? yaml><) (error "no yaml provided"))
	(define yaml (car yaml><))
	(define >< (libyaml-argparse (cdr yaml><) '() '(#:swap-when) #:no-clear))
	(let*
		(
			(?swap-when (assoc #:swap-when (cdr ><)))
			(swap-when (if ?swap-when
				(cdr ?swap-when)
				(lambda (l r) (string>? (->string (car l)) (->string (car r))))))
		)
		(define (sort <>)
			(define (insert ? <>)
				(define (:insert <l> ? <r>)
					(cond
						((null? <r>) (reverse (cons ? <l>)))
						((swap-when ? (car <r>)) (:insert (cons (car <r>) <l>) ? (cdr <r>)))
						(else (append (reverse <l>) (list ?) <r>))))
				(:insert '() ? <>))
			(define (:sort todo done)
				(cond
					((null? todo) done)
					(else (:sort (cdr todo) (insert (car todo) done)))))
			(:sort <> '()))
		(define (:mapping-ordered-yaml<- yaml)
			(cond
				((vector? yaml)
					(cond
						((= (vector-length yaml) 0) #())
						(else (list->vector (map :mapping-ordered-yaml<- (vector->list yaml))))))
				((null? yaml) '())
				((list? yaml) (map :mapping-ordered-yaml<- yaml))
				((pair? yaml)
					(cons
						(:mapping-ordered-yaml<- (car yaml))
						(:mapping-ordered-yaml<- (cdr yaml))))
				(else (if (procedure? yaml)
					(let ((yaml (yaml)))
						(:mapping-ordered-yaml<- (sort yaml)))
					yaml))))
		(:mapping-ordered-yaml<- yaml)))

(define (&in-yaml-mapping? == mapping key)
	(if (not (procedure? mapping))
		(error "try to find a key in a non mapping object" mapping))
	(let ((mapping (mapping)))
		(if (not (list mapping))
			(error "try to find a key in a non mapping object" mapping))
		(define (:&in-yaml-mapping? mapping)
			(cond
				((null? mapping) #f)
				((not (pair? (car mapping))) (:&in-yaml-mapping? (cdr mapping)))
				((== (car (car mapping)) key) (car mapping))
				(else (:&in-yaml-mapping? (cdr mapping)))))
		(:&in-yaml-mapping? mapping)))
(define (in-yaml-mapping? mapping key) (&in-yaml-mapping? equal? mapping key))
(define (in-yaml-mapping?? mapping key) (&in-yaml-mapping? eqv? mapping key))
(define (in-yaml-mapping??? mapping key) (&in-yaml-mapping? eq? mapping key))

(define (<-yaml . yaml><)
	(if (null? yaml><) (error "no yaml provided"))
	(let*
		(
			(yaml (car yaml><))
			(>< (libyaml-argparse
				(cdr yaml><)
				'()
				'(#:indent #:port #:encoding)
				#:no-clear
				))
			(?port (assoc #:port (cdr ><)))
			(port (if ?port (cdr ?port) (current-output-port)))
			(?encoding (assoc #:encoding (cdr ><)))
			(encoding (if ?encoding (cdr ?encoding) YAML_ANY_ENCODING))
			(memset (foreign-lambda c-pointer "memset" c-pointer int size_t))
			(&emitter (allocate (foreign-type-size "struct yaml_emitter_s")))
			(&event (allocate (foreign-type-size "struct yaml_event_s")))
			(clear (lambda ()
				(yaml_emitter_delete &emitter)
				(yaml_event_delete &event)
				(free &emitter)
				(free &event)
				(if ?port (close-output-port (cdr ?port)))))
			(error (lambda (^ . ...) (clear) (apply error (cons ^ ...))))
			;(?null (assoc #:null (cdr ><)))
			;(null (if ?null (cdr ?null) "~"))
		)
		(memset &event 0 (foreign-type-size "struct yaml_event_s"))
		(memset &emitter 0 (foreign-type-size "struct yaml_emitter_s"))

		(define-syntax <-* (syntax-rules ()
			((<-* emitter function event ...) (let ()
				(let ((<< (function event ...)))
					(cond ((not (= << 1))
						(error (sprintf "~S failed and return ~S"
							(quote function) <<)))))
				(let ((<< (yaml_emitter_emit emitter (car (list event ...)))))
					(cond ((not (= << 1))
						(error (sprintf "~S after ~S failed and return ~S"
							(quote yaml_emitter_emit)
							(quote function) <<)))))))))

		(yaml_emitter_initialize &emitter)
		(yaml_emitter_set_output_file &emitter port)
		(<-* &emitter yaml_stream_start_event_initialize &event encoding)
		(define (<-yaml-document yaml)
			(<-* &emitter yaml_document_start_event_initialize &event #f #f #f 0)
			(define (:<-yaml-in-document yaml)
				(cond
					((vector? yaml)
						(cond
							((= (vector-length yaml) 0)
								(<-* &emitter yaml_scalar_event_initialize
									&event #f #f "~" -1 1 1 YAML_PLAIN_SCALAR_STYLE))
							(else
								(error
									(string-append
										"A non 0 size vector was found in:"
										(sprintf "~%  ~S" yaml))))))
					;((null? yaml)
					;	(<-* &emitter yaml_scalar_event_initialize &event #f #f "~" -1 1 1
					;		YAML_PLAIN_SCALAR_STYLE))
					((procedure? yaml)
						(<-* &emitter yaml_mapping_start_event_initialize &event
							#f #f 0 YAML_BLOCK_MAPPING_STYLE)
						(let ((yaml (yaml)))
							(map
								(lambda (?) (:<-yaml-in-document (car ?)) (:<-yaml-in-document (cdr ?)))
								yaml))
						(<-* &emitter yaml_mapping_end_event_initialize &event)
					)
					((list? yaml)
						(<-* &emitter yaml_sequence_start_event_initialize &event
							#f #f 0 YAML_BLOCK_SEQUENCE_STYLE)
						(map :<-yaml-in-document yaml)
						(<-* &emitter yaml_sequence_end_event_initialize &event)
					)
					(else
					; Unknown error: if plain_implicit and quoted_implicit are both 0
					; yaml_scalar_event_initialize will get an double free error
						(cond
							((string? yaml)
								(let
									(
										(style (cond
											((< 1 (length (string-split yaml "\n" #t))) YAML_LITERAL_SCALAR_STYLE)
											(else YAML_PLAIN_SCALAR_STYLE)))
										(plain_implicit (if (string? (vector-ref (yaml<- `(#:input . ,yaml)) 0)) 1 0))
									)
									(<-* &emitter yaml_scalar_event_initialize
										&event #f #f yaml -1 plain_implicit 1 style)))
							((number? yaml)
								(cond
									((nan? yaml) (let ((scalar ".nan"))
										(<-* &emitter yaml_scalar_event_initialize &event #f #f scalar -1 1 1
											YAML_PLAIN_SCALAR_STYLE)))
									((infinite? yaml) (let ((scalar (if (> yaml 0) "+.inf" "-.inf")))
										(<-* &emitter yaml_scalar_event_initialize &event #f #f scalar -1 1 1
											YAML_PLAIN_SCALAR_STYLE)))
									(else (let ((scalar (number->string yaml)))
										(<-* &emitter yaml_scalar_event_initialize &event #f #f scalar -1 1 1
											YAML_PLAIN_SCALAR_STYLE)))))
							((boolean? yaml) (let ((scalar (if yaml "true" "false")))
								(<-* &emitter yaml_scalar_event_initialize &event #f #f scalar -1 1 1
									YAML_PLAIN_SCALAR_STYLE)))
						))))
			(:<-yaml-in-document yaml)
			(<-* &emitter yaml_document_end_event_initialize &event 0)
		)
		(map <-yaml-document (vector->list yaml)) ; XXX if map is parallel, emitter may be undefined
		(<-* &emitter yaml_stream_end_event_initialize &event)
		(clear)
	) ; let
)

)
