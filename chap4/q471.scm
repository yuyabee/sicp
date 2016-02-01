;; it is a memo, not an answer

;; when handling an infinite stream, it's required to use lazy evaluation
;; the purpose of adding interleave to append-stream was to avoid the case
;; that the first stream is infinite and it never reaches the second stream
