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

(include-relative "include/yaml2ss.ss")
;(include-relative "include/yaml2map-fixed-yaml.scm")
;(include-relative "include/if-in-yaml-map.scm")
;(include-relative "include/scm2yaml.scm")

)
