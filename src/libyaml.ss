(module (libyaml) ()

(import scheme (chicken module))

(define-syntax importrec (syntax-rules ()
	((importrec lib ...) (begin
		(import lib ...)(reexport lib ...)))))

(importrec (libyaml yaml.h))
(importrec (libyaml if))

(importrec (libyaml yaml2ss))
(importrec (libyaml ss2yaml))

)
