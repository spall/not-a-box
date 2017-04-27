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
			      (iota 2000))
		    (thread-rewind-receive (list "Msg1" "Msg2" "Msg3" "Msg4"))
		    (printf "Msg: ~a\n" (thread-receive))
		    ))
   
   ;; a reader thread
   (define reader (lambda ()
		    (for-each (lambda (v)
				(if (equal? v 999)
				    (printf "Value: ~a\n" (unbox b))
				    (void)))
			      (iota 2000))
		    (printf "Current thread: ~a\n" (current-thread))
		    (kill-thread (current-thread))
		    (printf "Msg: ~a\n" (thread-receive))
		    (printf "Msg: ~a\n" (thread-try-receive))
		    (printf "Msg: ~a\n" (thread-receive))
		    
		    ))
 
   (define wthread (thread writer))
   (define rthread (thread/suspend-to-kill reader))

   (thread-send (current-thread) "Msgg")
   
  (printf "thread? ~a\n" (thread? wthread))
  (printf "thread? ~a\n" (thread? rthread))

  (thread-send rthread "Msg1")

  (printf "Current thread: ~a\n" (current-thread))

  (thread-suspend wthread)
  (printf "Running? ~a\n" (thread-running? wthread))
  (thread-resume wthread)
  (printf "Running? ~a\n" (thread-running? wthread))

  (thread-send rthread "Msg2")
  (thread-receive)

  (printf "Terminating\n")

  ;; run a benchmark inside thread to see how it performs compared to normal racket threads.

  ;; try thread-ring.rkt benchmark.
  ;; just put a time call around it.
  ;; change engine run time to see what is fastest.
  
  ))

#|
  
   (kill-thread rthread) 
   
   (printf "dead? ~a\n" (thread-dead? rthread))
   (thread-resume rthread)
   (printf "running? ~a\n" (thread-running? rthread))
   
   
   How to implement sleep: have sorted list of deadlines. and wakeup threads that qualify....  

   
   
   
|#
