(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
		 ; the argstreams can be plural so have to use map
		 ; so it is applying multiple times
			(apply proc (map stream-car argstreams))
			(apply stream-map
						 (cons proc (map stream-cdr argstreams))))))
