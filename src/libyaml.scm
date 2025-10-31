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

(include-relative "include/2yaml.scm")
(include-relative "include/2map-fixed-yaml.scm")

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

;(define (fixed-mapping yaml)
;	(cond
;		((list? yaml) (map fixed-mapping yaml))
;		((pair? yaml) (cons (fixed-mapping (car yaml)) (fixed-mapping (cdr yaml))))
;		(else (if (procedure? yaml) (fixed-mapping (yaml)) yaml))))


(define (&in-yaml-map? == mapping key)
	(if (not (procedure? mapping))
		(error "try to find a key in a non mapping object" mapping))
	(let ((mapping (mapping)))
		(if (not (list mapping))
			(error "try to find a key in a non mapping object" mapping))
		(define (:&in-yaml-map? mapping)
			(cond
				((null? mapping) #f)
				((not (pair? (car mapping))) (:&in-yaml-map? (cdr mapping)))
				((== (car (car mapping)) key) (car mapping))
				(else (:&in-yaml-map? (cdr mapping)))))
		(:&in-yaml-map? mapping)))
(define (in-yaml-map? mapping key) (&in-yaml-map? equal? mapping key))
(define (in-yaml-map?? mapping key) (&in-yaml-map? eqv? mapping key))
(define (in-yaml-map??? mapping key) (&in-yaml-map? eq? mapping key))

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
