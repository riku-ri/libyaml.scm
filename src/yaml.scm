(import scheme)
(import (chicken foreign))
(import (chicken format))
(import (chicken irregex))

(foreign-declare "#include <string.h>")
(foreign-declare "#include <yaml.h>")

(define (assq.  key <assq>) (cdr (assq  key <assq>))) ; TODO: Maybe test only. This definition should not be in the top-level
(define (assv.  key <assq>) (cdr (assv  key <assq>))) ; TODO: Maybe test only. This definition should not be in the top-level
(define (assoc. key <assq>) (cdr (assoc key <assq>))) ; TODO: Maybe test only. This definition should not be in the top-level

(define-foreign-type yaml_error_type_t int)
; XXX: Here use int as return value is because the typedef cannot be set in (foreigin-lambda*)
(define YAML_NO_ERROR (foreign-value "YAML_NO_ERROR" yaml_error_type_t))
(define YAML_MEMORY_ERROR (foreign-value "YAML_MEMORY_ERROR" yaml_error_type_t))
(define YAML_READER_ERROR (foreign-value "YAML_READER_ERROR" yaml_error_type_t))
(define YAML_SCANNER_ERROR (foreign-value "YAML_SCANNER_ERROR" yaml_error_type_t))
(define YAML_PARSER_ERROR (foreign-value "YAML_PARSER_ERROR" yaml_error_type_t))
(define YAML_COMPOSER_ERROR (foreign-value "YAML_COMPOSER_ERROR" yaml_error_type_t))
(define YAML_WRITER_ERROR (foreign-value "YAML_WRITER_ERROR" yaml_error_type_t))
(define YAML_EMITTER_ERROR (foreign-value "YAML_EMITTER_ERROR" yaml_error_type_t))
; TODO: Such definition can be done with libclang<-yaml.h and m4-like

(define-foreign-type yaml_encoding_t int)
(define YAML_ANY_ENCODING (foreign-value "YAML_ANY_ENCODING" yaml_encoding_t))
(define YAML_UTF8_ENCODING (foreign-value "YAML_UTF8_ENCODING" yaml_encoding_t))
(define YAML_UTF16LE_ENCODING (foreign-value "YAML_UTF16LE_ENCODING" yaml_encoding_t))
(define YAML_UTF16BE_ENCODING (foreign-value "YAML_UTF16BE_ENCODING" yaml_encoding_t))

(define-foreign-type yaml_event_type_t int)
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
;(define yaml_parser_set_input_string (foreign-lambda void "yaml_parser_set_input_string" (c-pointer "yaml_parser_t") c-string size_t))
; Use (open-input-string) if read from string
(define yaml_parser_parse (foreign-lambda int "yaml_parser_parse" (c-pointer "yaml_parser_t") (c-pointer "yaml_event_t")))
(define yaml_event_delete (foreign-lambda void "yaml_event_delete" (c-pointer "yaml_event_t")))
(define yaml_parser_delete (foreign-lambda void "yaml_parser_delete" (c-pointer "yaml_parser_t")))
(define yaml-event->scalar.value (foreign-lambda* (c-pointer "yaml_char_t") (((c-pointer "yaml_event_t") yaml_event_p)) "C_return((yaml_event_p)->data.scalar.value);"))

(define (read-yaml . <optional>)
	(let*
		(
			(memset (foreign-lambda c-pointer "memset" c-pointer int size_t))

			(&parser (foreign-lambda* (c-pointer "yaml_parser_t") () "static yaml_parser_t yaml_parser; C_return(&yaml_parser);"))
			(&event (foreign-lambda* (c-pointer "yaml_event_t") () "static yaml_event_t yaml_event; C_return(&yaml_event);"))

			(type<- (foreign-lambda* yaml_event_type_t (((c-pointer "yaml_event_t") _p)) "C_return((_p)->type);"))
			(error<- (foreign-lambda* yaml_error_type_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->error);"))
			(problem<- (foreign-lambda* c-string (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->problem);"))
			(context<- (foreign-lambda* c-string (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->context);"))
			(problem_mark.line<- (foreign-lambda* size_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->problem_mark.line);"))
			(problem_mark.column<- (foreign-lambda* size_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->problem_mark.column);"))
			(data.scalar.value<- (foreign-lambda* c-string (((c-pointer "yaml_event_t") _p)) "C_return((_p)->data.scalar.value);"))

			($enum ; construct an association list that contain (#:string . (symbol->string))
				((lambda ($) ($ $))
					(lambda ($unit) (lambda (<enum>)
						(cond
							((null? <enum>) '())
							(else (cons `(,(eval (car <enum>)) (#:string . ,(symbol->string (car <enum>)))) (($unit $unit) (cdr <enum>)))))))))

			(yaml_error_type_e ($enum '(
				YAML_NO_ERROR
				YAML_MEMORY_ERROR
				YAML_READER_ERROR
				YAML_SCANNER_ERROR
				YAML_PARSER_ERROR
				YAML_COMPOSER_ERROR
				YAML_WRITER_ERROR
				YAML_EMITTER_ERROR
			)))
			(yaml_encoding_e ($enum '(
				YAML_ANY_ENCODING
				YAML_UTF8_ENCODING
				YAML_UTF16LE_ENCODING
				YAML_UTF16BE_ENCODING
			)))
		)
		(if (assoc #:port <optional>) (if (string? (cdr (assoc #:port <optional>))) (error "Use (open-input-string) instead")))
		(current-input-port (if (assoc #:port <optional>) (cdr (assoc #:port <optional>)) (current-input-port)))
		(memset (&parser) 0 (foreign-type-size "yaml_parser_t"))
		(memset (&event) 0 (foreign-type-size "yaml_event_t"))

		(let* ((yaml_parser_initialize<- (yaml_parser_initialize (&parser))))
			(if (not (= 1 yaml_parser_initialize<-))
				(error (sprintf "[~A] ~A" yaml_parser_initialize<- (error<- (&parser))))))
		(yaml_parser_set_input_file (&parser) (current-input-port))
		(yaml_parser_set_encoding (&parser) (if (assoc #:encoding <optional>) (cdr (assoc #:encoding <optional>)) YAML_ANY_ENCODING))
		(if (assoc #:encoding <optional>)
			(assert (=
				((foreign-lambda* yaml_encoding_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->encoding);") (&parser))
				(cdr (assoc #:encoding <optional>)))))
		;(write (assq. #:string (assq. ((foreign-lambda* yaml_encoding_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->encoding);") (&parser)) yaml_encoding_e)))
		(define (>read-yaml)
			(if (not (list? <optional>)) (error "paramters to (read-yaml) must be a association list"))
			(let*
				(

					($before-ending (lambda () (close-input-port (current-input-port)) (yaml_event_delete (&event)) (yaml_parser_delete (&parser))))

					($error (lambda (s) (error (sprintf "You should never go into this event [~A]" s))))
					(yaml_event_type_e ($enum '(
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
				)
				(cond
					((not (= (yaml_parser_parse (&parser) (&event)) 1)) ; According to comment in yaml.h , yaml_parser_parse() return 1 if the function succeeded
						(let*
							(
								(errmessage (sprintf
									"[~A] ~A ~A at [line:~A , colunm:~A]"
									(error<- (&parser))
									(problem<- (&parser))
									(if (context<- (&parser)) (context<- (&parser)) "")
									(+ 1 (problem_mark.line<- (&parser)))
									(+ 1 (problem_mark.column<- (&parser)))))
							)
						($before-ending)
						(error errmessage)))
					(else
						(write "here")
						(print (assoc #:string (cdr (assoc (type<- (&event)) yaml_event_type_e))))
						;(let ((v ((cdr (assoc #:lambda (cdr (assoc (type<- (&event)) yaml_event_type_e))))))) (print v) v)
						;(print (assoc #:string (cdr (assoc (type<- (&event)) yaml_event_type_e))))
						(cond
							((= YAML_SCALAR_EVENT (type<- (&event))) (data.scalar.value<- (&event))) ; TODO: distinguish var values
							;((= YAML_STREAM_END_EVENT (type<- (&event))) '())
							((or (= YAML_STREAM_START_EVENT (type<- (&event))) (= YAML_STREAM_END_EVENT (type<- (&event))))
								(((lambda ($) ($ $))
									(lambda (R)
										(lambda (@)
											(cond
												((= YAML_STREAM_END_EVENT (type<- (&event))) (reverse @))
												(else ((R R) (cons (>read-yaml) @)))))))
								'()))
							((or (= YAML_DOCUMENT_START_EVENT (type<- (&event))) (= YAML_DOCUMENT_END_EVENT (type<- (&event))))
								(((lambda ($) ($ $))
									(lambda (R)
										(lambda ()
											(cond
												((= YAML_DOCUMENT_END_EVENT (type<- (&event))) '())
												(else ((R R)))))))
								))
						)
					)
				)
			))
			(>read-yaml)))

(write (read-yaml))
