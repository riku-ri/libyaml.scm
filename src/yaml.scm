(import scheme)
(import (chicken foreign))
(import (chicken format))

(foreign-declare "#include <string.h>")
(foreign-declare "#include <yaml.h>")
(foreign-declare "yaml_parser_t PARSER;yaml_event_t EVENT;")

(define yaml-parser (foreign-lambda* (c-pointer "yaml_parser_t") () "static yaml_parser_t yaml_parser; C_return(&yaml_parser);"))
(define yaml-event (foreign-lambda* (c-pointer "yaml_event_t") () "static yaml_event_t yaml_event; C_return(&yaml_event);"))

(define memset (foreign-lambda c-pointer "memset" c-pointer int size_t))

(define yaml_parser_initialize (foreign-lambda int "yaml_parser_initialize" (c-pointer "yaml_parser_t")))
(define yaml_parser_set_input_file (foreign-lambda void "yaml_parser_set_input_file" (c-pointer "yaml_parser_t") (c-pointer "FILE")))
(define yaml_parser_set_input_string (foreign-lambda void "yaml_parser_set_input_string" (c-pointer "yaml_parser_t") nonnull-c-string size_t))
(define yaml_parser_parse (foreign-lambda int "yaml_parser_parse" (c-pointer "yaml_parser_t") (c-pointer "yaml_event_t")))
(define yaml_event_delete (foreign-lambda void "yaml_event_delete" (c-pointer "yaml_event_t")))
(define yaml_parser_delete (foreign-lambda void "yaml_parser_delete" (c-pointer "yaml_parser_t")))
(define yaml-event->scalar.value (foreign-lambda* (c-pointer "yaml_char_t") (((c-pointer "yaml_event_t") yaml_event_p)) "C_return((yaml_event_p)->data.scalar.value);"))
;(define-foreign-type yaml_event_type_t "yaml_event_type_t")
(define-foreign-type yaml_error_type_t int)
(define-foreign-type yaml_event_type_t int)
; XXX: Here use int as return value is because the typedef "yaml_event_type_t" cannot be set in (foreigin-safe-lambda*)
;(define yaml-event->type (foreign-lambda* int (((c-pointer "yaml_event_t") _p)) "C_return((_p)->type);"))

(define (read-yaml-port . <-port)
	(if (not (null? <-port)) (current-input-port (car <-port))) ;implicit (input-port?) check in (current-input-port)
	(memset (yaml-parser) 0 (foreign-type-size "yaml_parser_t"))
	(memset (yaml-event) 0 (foreign-type-size "yaml_event_t"))
	(yaml_parser_initialize (yaml-parser))
	(yaml_parser_set_input_file (yaml-parser) (current-input-port))

	(define (:read-yaml-port @ parser event port)
		(cond
			((not (= 1 (yaml_parser_parse (yaml-parser) (yaml-event))))
				(error (sprintf
					"[~A] ~A ~A at [line:~A , colunm:~A]"
					((foreign-lambda* yaml_error_type_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->error);") (yaml-parser))
					((foreign-lambda* nonnull-c-string (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->problem);") (yaml-parser))
					(if ((foreign-lambda* nonnull-c-string (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->context);") (yaml-parser))
						((foreign-lambda* nonnull-c-string (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->context);") (yaml-parser))
						"")
					(+ 0 ((foreign-lambda* size_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->problem_mark.line);") (yaml-parser)))
					(+ 1 ((foreign-lambda* size_t (((c-pointer "yaml_parser_t") _p)) "C_return((_p)->problem_mark.column);") (yaml-parser)))
				)))
			(else
				;(print (=
				;	((foreign-lambda* int (((c-pointer "yaml_event_t") _p)) "C_return((_p)->type);") (yaml-event))
				;	(foreign-value "(YAML_STREAM_START_EVENT)" int)
				;))
				(:read-yaml-port @ parser event port)
			)))
	(:read-yaml-port "" (yaml-parser) (yaml-event) <-port))

(set! f (open-input-file "/home/luli/tmp/tmp.yaml"))
(print (read-yaml-port f))
(close-input-port f)
