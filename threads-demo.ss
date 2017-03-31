(import (core))

;; make some threads
;; that share some memory
;; and do things.

(define b (box 0))

;; a writer thread
(define writer (lambda () 
		 (for-each (lambda (v)
			     (printf "running\n")
			     (set-box! b v))
			   '(1 2 3 4 5 6 7 8 9 10))))

;; a reader thread
(define reader (lambda ()
		 (for-each (lambda (v)
			     (printf "~a\n" (unbox b)))
			   '(1 2 3 4 5 6 7 8 9 10)))) ;; just do it 10 times.

(printf "Creating a thread\n")
(define wthread (thread writer))
(define rthread (thread reader))


(printf "thread? ~a\n" (thread? wthread))
(printf "thread? ~a\n" (thread? rthread))
(current-thread)

(printf "start other direction\n")
(thread reader)
(thread writer)
