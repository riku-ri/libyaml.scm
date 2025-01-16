(import scheme)
(import (chicken foreign))
(import (chicken format))

(define (assq. key <list>) (cdr (assq key <list>)))

(foreign-declare "#include <string.h>")
(foreign-declare "#include <yaml.h>")

;(define-foreign-type yaml_event_type_t "yaml_event_type_t")
(define-foreign-type yaml_event_type_t int)
; XXX: Here use int as return value is because the typedef "yaml_event_type_t" cannot be set in (foreigin-safe-lambda*)
(define-foreign-type yaml_error_type_t int)
(define-foreign-type yaml_encoding_t int)

(define YAML_ANY_ENCODING (foreign-value "(YAML_ANY_ENCODING)" yaml_encoding_t))
(define YAML_UTF8_ENCODING (foreign-value "(YAML_UTF8_ENCODING)" yaml_encoding_t))
(define YAML_UTF16LE_ENCODING (foreign-value "(YAML_UTF16LE_ENCODING)" yaml_encoding_t))
(define YAML_UTF16BE_ENCODING (foreign-value "(YAML_UTF16BE_ENCODING)" yaml_encoding_t))

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
(define <yaml-event-type> (list
	(cons YAML_NO_EVENT (list '(#:name . "YAML_NO_EVENT")))
	(cons YAML_STREAM_START_EVENT (list '(#:name . "YAML_STREAM_START_EVENT")))
	(cons YAML_STREAM_END_EVENT (list '(#:name . "YAML_STREAM_END_EVENT")))
	(cons YAML_DOCUMENT_START_EVENT (list '(#:name . "YAML_DOCUMENT_START_EVENT")))
	(cons YAML_DOCUMENT_END_EVENT (list '(#:name . "YAML_DOCUMENT_END_EVENT")))
	(cons YAML_ALIAS_EVENT (list '(#:name . "YAML_ALIAS_EVENT")))
	(cons YAML_SCALAR_EVENT (list '(#:name . "YAML_SCALAR_EVENT")))
	(cons YAML_SEQUENCE_START_EVENT (list '(#:name . "YAML_SEQUENCE_START_EVENT")))
	(cons YAML_SEQUENCE_END_EVENT (list '(#:name . "YAML_SEQUENCE_END_EVENT")))
	(cons YAML_MAPPING_START_EVENT (list '(#:name . "YAML_MAPPING_START_EVENT")))
	(cons YAML_MAPPING_END_EVENT (list '(#:name . "YAML_MAPPING_END_EVENT")))
))

(define yaml-parser (foreign-lambda* (c-pointer "yaml_parser_t") () "static yaml_parser_t yaml_parser; C_return(&yaml_parser);"))
(define yaml-event (foreign-lambda* (c-pointer "yaml_event_t") () "static yaml_event_t yaml_event; C_return(&yaml_event);"))

(define memset (foreign-lambda c-pointer "memset" c-pointer int size_t))

(define yaml_parser_initialize (foreign-lambda int "yaml_parser_initialize" (c-pointer "yaml_parser_t")))
(define yaml_parser_set_input_file (foreign-lambda void "yaml_parser_set_input_file" (c-pointer "yaml_parser_t") (c-pointer "FILE")))
(define yaml_parser_set_input_string (foreign-lambda void "yaml_parser_set_input_string" (c-pointer "yaml_parser_t") c-string size_t))
(define yaml_parser_parse (foreign-lambda int "yaml_parser_parse" (c-pointer "yaml_parser_t") (c-pointer "yaml_event_t")))
(define yaml_event_delete (foreign-lambda void "yaml_event_delete" (c-pointer "yaml_event_t")))
(define yaml_parser_delete (foreign-lambda void "yaml_parser_delete" (c-pointer "yaml_parser_t")))
(define yaml-event->scalar.value (foreign-lambda* (c-pointer "yaml_char_t") (((c-pointer "yaml_event_t") yaml_event_p)) "C_return((yaml_event_p)->data.scalar.value);"))

(define (read-yaml-port . <-port)
	(if (not (null? <-port)) (current-input-port (car <-port))) ;implicit (input-port?) check in (current-input-port)
	(memset (yaml-parser) 0 (foreign-type-size "yaml_parser_t"))
	(memset (yaml-event) 0 (foreign-type-size "yaml_event_t"))
	(yaml_parser_initialize (yaml-parser))
	(yaml_parser_set_input_file (yaml-parser) (current-input-port))

	(define (>read-yaml-port >@ >parser >event >port)
		(let
			((yaml_parser_parse<- (yaml_parser_parse (yaml-parser) (yaml-event))))
			(cond
				((not (= yaml_parser_parse<- 1)) ; According to comment in yaml.h , yaml_parser_parse() return 1 if the function succeeded
					(close-input-port (current-input-port))
					(let*
						(
							(error<- (foreign-lambda* yaml_error_type_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->error);"))
							(problem<- (foreign-lambda* c-string (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->problem);"))
							(context<- (foreign-lambda* c-string (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->context);"))
							(problem_mark.line<- (foreign-lambda* size_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->problem_mark.line);"))
							(problem_mark.column<- (foreign-lambda* size_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->problem_mark.column);"))
							(
								errmessage
								(sprintf
									"[~A] ~A ~A at [line:~A , colunm:~A]"
									(error<- (yaml-parser))
									(problem<- (yaml-parser))
									(if (context<- (yaml-parser)) (context<- (yaml-parser)) "")
									(+ 0 (problem_mark.line<- (yaml-parser)))
									(+ 1 (problem_mark.column<- (yaml-parser))))
							)
						)
						(yaml_event_delete (yaml-event))
						(yaml_parser_delete (yaml-parser))
						(error errmessage)))
				(
					(=
						YAML_STREAM_END_EVENT
						((foreign-lambda* yaml_event_type_t (((c-pointer "yaml_event_t") _p)) "C_return((_p)->type);") (yaml-event))
					)
					(close-input-port (current-input-port)))
				(else
					(let*
						(
							(type<- (foreign-lambda* yaml_event_type_t (((c-pointer "yaml_event_t") _p)) "C_return((_p)->type);"))
							(data.scalar.value<- (foreign-lambda* c-string (((c-pointer "yaml_event_t") _p)) "C_return((_p)->data.scalar.value);"))
						)
						(cond
							((= YAML_SCALAR_EVENT (type<- (yaml-event)))
								(print* (string-append
									(assq. #:name (assq. (type<- (yaml-event)) <yaml-event-type>)) ": \""
									(data.scalar.value<- (yaml-event)) "\"\n") ""))
							(else
								(print (assq. #:name (assq. (type<- (yaml-event)) <yaml-event-type>)))))
						(>read-yaml-port >@ >parser >event >port))))))
	(>read-yaml-port "" (yaml-parser) (yaml-event) <-port))

(read-yaml-port)
