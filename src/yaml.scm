(import scheme)
(import (chicken foreign))
(import (chicken format))

(foreign-declare "#include <string.h>")
(foreign-declare "#include <yaml.h>")

(define yaml-parser (foreign-safe-lambda* (c-pointer "yaml_parser_t") () "static yaml_parser_t yaml_parser; C_return(&yaml_parser);"))
(define yaml-event (foreign-safe-lambda* (c-pointer "yaml_event_t") () "static yaml_event_t yaml_event; C_return(&yaml_event);"))

(define memset (foreign-safe-lambda c-pointer "memset" c-pointer int size_t))
(define yaml_parser_set_input_file (foreign-safe-lambda void "yaml_parser_set_input_file" (c-pointer "yaml_parser_t") (c-pointer "FILE")))
(define yaml_parser_parse (foreign-safe-lambda int "yaml_parser_parse" (c-pointer "yaml_parser_t") (c-pointer "yaml_event_t")))
(define yaml_event_delete (foreign-safe-lambda void "yaml_event_delete" (c-pointer "yaml_event_t")))
(define yaml_parser_delete (foreign-safe-lambda void "yaml_parser_delete" (c-pointer "yaml_parser_t")))
(define yaml-event->scalar.value (foreign-safe-lambda* (c-pointer "yaml_char_t") (((c-pointer "yaml_event_t") event_p)) "C_return((event_p)->data.scalar.value);"))
;(define-foreign-type yaml_event_type_t "yaml_event_type_t")
(define-foreign-type yaml_event_type_t int)
; XXX: Here use int as return value is because the typedef "yaml_event_type_t" cannot be set in (foreigin-safe-lambda*)
(define yaml-event->type (foreign-safe-lambda* int (((c-pointer "yaml_event_t") event_p)) "C_return((event_p)->type);"))

(print (yaml-parser))
(print (yaml-parser))
(print (yaml-parser))

;(define (read-yaml-port . <-port)
;	(if (not (null? <-port)) (current-input-port (car <-port))) ;implicit (input-port?) check in (current-input-port)
;	(memset (yaml-parser) 0 (foreign-type-size "yaml_parser_t"))
;	(memset (yaml-event) 0 (foreign-type-size "yaml_event_t"))
;	(define (:read-yaml-port @ parser event port)
;		(cond
;			((not (= 0 (yaml_parser_parse (yaml-parser) (yaml-event))))
;				(print-call-chain (current-error-port))

;(set! f (open-input-file "/home/luli/tmp/tmp.yaml"))
;(print (read-yaml-port f))
;(close-input-port f)
