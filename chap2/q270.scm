(define rock-code '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define rock-huffman-tree (generate-huffman-tree rock-code))

(define rock-song '(GET A JOB
                        SHA NA NA NA NA NA NA NA NA
                        GET A JOB
                        SHA NA NA NA NA NA NA NA NA
                        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                        SHA BOOM))

(define rock-song-bit (encode rock-song rock-huffman-tree))

(length rock-song-bit)

(length rock-song)
