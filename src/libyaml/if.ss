(module (libyaml if) *

(import scheme (chicken base))
(import (chicken condition))

(define (yaml? ?)
	(and
		(procedure? ?)
		(handle-exceptions e #f (list? (? -1)))
		(handle-exceptions e #f (if (?) #t))
		; only catch exception here
		; just keep always true if no exception
		(equal? (car (? -1)) (?))
		(foldl (lambda (l r) (and l r)) #t
			(map
				((lambda (@) (@ @)) (lambda (@) (lambda (?)
					(or (ylist? ?) (ymap? ?) (yscalar? ?)))))
				(? -1)
			)
		)
	))

(define (ylist? ?)
	(and
		(vector? ?)
		(foldl (lambda (l r) (and l r)) #t
			(map
				((lambda (@) (@ @)) (lambda (@) (lambda (?)
					(or (ylist? ?) (ymap? ?) (yscalar? ?)))))
				(vector->list ?)
			)
		)
	))

(define (ymap? ?)
	(and
		(list? ?)
		(= 1 (length ?))
		(list? (car ?))
		(let ((?alist (car ?))) (and
				(foldl (lambda (l r) (and l r)) #t (map pair? ?alist))
				(foldl (lambda (l r) (and l r)) #t (map
					((lambda (@) (@ @)) (lambda (@) (lambda (?)
						(and
							(or (ylist? (car ?)) (ymap? (car ?)) (yscalar? (car ?)))
							(or (ylist? (cdr ?)) (ymap? (cdr ?)) (yscalar? (cdr ?)))
						))))
					?alist
				))
			))
	))

(define (yscalar? ?)
	(or
		(null? ?)
		(boolean? ?)
		(number? ?)
		(string? ?)
	))

) ;module
