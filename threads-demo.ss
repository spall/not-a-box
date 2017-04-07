(import (core))

(start-program 
 (lambda ()
   
   ;; make some threads
   ;; that share some memory
   ;; and do things.
   
   (define b (box 0))
   
   ;; a writer thread
   (define writer (lambda () 
		    (for-each (lambda (v)
				(set-box! b v))
			      (iota 2000))))
   
   ;; a reader thread
   (define reader (lambda ()
		    (for-each (lambda (v)
				(if (equal? v 999)
				    (printf "Value: ~a\n" (unbox b))
				    (void)))
			      (iota 2000))))
 
   (define wthread (thread writer))
   (define rthread (thread/suspend-to-kill reader))
   
   (printf "thread? ~a\n" (thread? wthread))
   (printf "thread? ~a\n" (thread? rthread))
   ))

#|
  
   
   (thread-send wthread "Msg1" #f)
   (thread-send rthread "Msg2" #f)
   
   (printf "Msg: ~a\n" (thread-receive))
   (printf "Msg: ~a\n" (thread-try-receive))
   
   (thread-rewind-receive (list "Msg1" "Msg2" "Msg3" "Msg4"))
   (thread-suspend wthread) 
   (thread-resume wthread) 
   
   (printf "Msg: ~a\n" (thread-try-receive))
   (kill-thread rthread) 
   
   (printf "dead? ~a\n" (thread-dead? rthread))
   (thread-resume rthread)
   (printf "running? ~a\n" (thread-running? rthread))
   
   
   
   
   (printf "Current thread: ~a\n" (current-thread))))
   
|#
