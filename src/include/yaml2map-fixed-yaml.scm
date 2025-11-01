(include-relative "foreign.scm")
(include-relative "../varg.ss/src/include/varg.ss")
(foreign-declare "#include <yaml.h>")

(define (map-fixed-yaml<- . <>)
	(define errtag "[map-fixed-yaml<-]")
	(define inerr "internal logic error, please contact maintainer")

	(define (assoc* if-not-in key alist)
		(cond ((assoc key alist) (cdr (assoc key alist))) (else if-not-in)))
	(define >< (varg <> '(#:with-value #:swap-when) '(#:literal yaml)))
	(define yaml (car (cdr (assoc #:literal ><))))
	(let*
		(
			(with-value (cdr (assoc #:with-value ><)))
			(swap-when (cond
				((assoc #:swap-when with-value) (cdr (assoc #:swap-when with-value)))
				(else (lambda (l r) (string>? (->string (car l)) (->string (car r)))))))
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
		(define (:map-fixed-yaml<- yaml)
			(cond
				((vector? yaml)
					(cond
						((= (vector-length yaml) 0) #())
						(else (error (sprintf "non 0 size vector found:~%  ~S" yaml)))))
				((null? yaml) '())
				((list? yaml) (map :map-fixed-yaml<- yaml))
				((pair? yaml)
					(cons
						(:map-fixed-yaml<- (car yaml))
						(:map-fixed-yaml<- (cdr yaml))))
				(else (if (procedure? yaml)
					(let ((yaml (yaml)))
						(:map-fixed-yaml<- (sort yaml)))
					yaml))))
		(list->vector (map :map-fixed-yaml<- (vector->list yaml)))))

;(define (fixed-mapping yaml)
;	(cond
;		((list? yaml) (map fixed-mapping yaml))
;		((pair? yaml) (cons (fixed-mapping (car yaml)) (fixed-mapping (cdr yaml))))
;		(else (if (procedure? yaml) (fixed-mapping (yaml)) yaml))))

