(import libyaml)
(import test)


(test-group "yaml<-"
	(test #t (procedure? (yaml<- "")))
	(test #t (procedure?
			(with-input-from-file "/dev/null" (lambda () (yaml<- (current-input-port))))))
	(test-group "data"
		(test-group "scalar"
			(test-group "null"
				(test #t (null? (((yaml<- "~")))))
				(test #t (null?
					((with-input-from-file "/dev/null" (lambda () (yaml<- (current-input-port)))))))
			)
		)
	)
)
