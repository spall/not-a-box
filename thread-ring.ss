(import (core))

 (start-program
  (lambda ()
    
    ;; The Computer Language Benchmarks Game
    ;; http://shootout.alioth.debian.org/
    ;;
    ;; Uses Racket threads
    
    ;; Each thread runs this loop:
    (define (run id next)
      (let ([v (thread-receive)])
	(cond
	 [(zero? v) ;; Done
	  (printf "~a\n" id)
	  (exit)]
	 [else ;; Keep going
	  (thread-send next (sub1 v))
	  (run id next)])))
    
    (define (create-thread next id)
      (if (<= id 0)
	  next
	  (create-thread 
	   (thread (lambda () (run id next)))
	   (- id 1))))
    
    (let ([n 1000000])
      (let ([t1 (create-thread (current-thread) 502)])
	(thread-send t1 n)
	(run 503 t1)))
    
    
    ))
