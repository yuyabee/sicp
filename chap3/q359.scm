(load "./chap3/stream.scm")

(define (integrate-series s)
	(stream-map / s integers))
