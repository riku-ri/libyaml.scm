(module (libyaml ss2yaml)
	(
		<-yaml
	)

(import scheme)
(import (chicken base))
(import (chicken string))
(import (chicken memory))
(import (chicken foreign))
(import (chicken format))
(import (chicken condition))
(import (libyaml yaml.h))
(import (libyaml if))
(import (libyaml yaml2ss))
(import varg)

(foreign-declare "#include <yaml.h>")

(define-foreign-type enum int)

(define (<-yaml . <>) (let-syntax
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
		(>< (varg
			<>
			'(#:literal yaml)
			'(#:with-value #:indent #:port #:encoding)
			'(#:without-value
				#:close-output-port
				#:strict-input
			)
		))
		(?close-output-port
			(member #:close-output-port (cdr (assoc #:without-value ><))))
		(?strict-input
			(member #:strict-input (cdr (assoc #:without-value ><))))
		(yaml (let ((yaml (car (cdr (assoc #:literal ><))))) (cond
			(?strict-input (cond
				((yaml? yaml) (yaml -1))
			))
			(else yaml)
		)))
	) (let*
	(
		(port (let ((with-value (cdr (assoc #:with-value ><))))
			(cond
				((assoc #:port with-value)
					(if (not (output-port? (cdr (assoc #:port with-value))))
						(abort (ycondition
							'message (sprintf
								"#:port is not a output port:\n~S"
								(cdr (assoc #:port with-value))))))
					(cdr (assoc #:port with-value)))
				(else (current-output-port)))))
		(encoding (let ((with-value (cdr (assoc #:with-value ><))))
			(cond
				((assoc #:encoding with-value) (cdr (assoc #:encoding with-value)))
				(else YAML_ANY_ENCODING))))
		(memset (foreign-lambda c-pointer "memset" c-pointer int size_t))
		(&emitter (allocate (foreign-type-size "struct yaml_emitter_s")))
		(&event (allocate (foreign-type-size "struct yaml_event_s")))
		(close (lambda ()
			(yaml_emitter_delete &emitter)
			(yaml_event_delete &event)
			(free &emitter)
			(free &event)
			(if ?close-output-port (close-output-port port))
		))
	) (let-syntax
	(
		(abort (syntax-rules ()
			((abort condition ...) (begin
				(close)
				(abort condition ...)))))
	)
	(memset &event 0 (foreign-type-size "struct yaml_event_s"))
	(memset &emitter 0 (foreign-type-size "struct yaml_emitter_s"))

	(define-syntax <-* (syntax-rules ()
		((<-* emitter function event ...) (let ()
			(let ((<< (function event ...)))
				(cond ((not (= << 1))
					(abort (ycondition 'message (sprintf "~S failed and return ~S"
						(quote function) <<))))))
			(let ((<< (yaml_emitter_emit emitter (car (list event ...)))))
				(cond ((not (= << 1))
					(abort (ycondition 'message (sprintf "~S after ~S failed and return ~S"
						(quote yaml_emitter_emit)
						(quote function) <<))))))))))

	(yaml_emitter_initialize &emitter)
	(yaml_emitter_set_output_file &emitter port)
	(<-* &emitter yaml_stream_start_event_initialize &event encoding)
	; XXX: encoding would not make exception or
	;      make struct content different inside yaml/libyaml
	;      so here did not check the error
	;      but invalid encoding may generate undefined character
	(define (<-yaml-document yaml)
		(<-* &emitter yaml_document_start_event_initialize &event #f #f #f 0)
		(define (:<-yaml-in-document yaml)
			(cond
				((null? yaml)
					(<-* &emitter yaml_scalar_event_initialize &event #f #f "~" -1 1 1
						YAML_PLAIN_SCALAR_STYLE))
				((ymap? yaml)
					(<-* &emitter yaml_mapping_start_event_initialize &event
						#f #f 0 YAML_BLOCK_MAPPING_STYLE)
					(let ((alist (car yaml)))
						(map
							(lambda (?)
								(:<-yaml-in-document (car ?))
								(:<-yaml-in-document (cdr ?)))
							alist))
					(<-* &emitter yaml_mapping_end_event_initialize &event)
				)
				((ylist? yaml)
					(<-* &emitter yaml_sequence_start_event_initialize &event
						#f #f 0 YAML_BLOCK_SEQUENCE_STYLE)
					(map :<-yaml-in-document (vector->list yaml))
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
									(plain_implicit (if (string? ((yaml<- yaml))) 1 0))
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
	(map <-yaml-document yaml)
	; XXX if map is parallel, emitter may be undefined
	(<-* &emitter yaml_stream_end_event_initialize &event)
	(close)
)))))

) ;module
