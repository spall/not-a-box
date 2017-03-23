;; https://docs.racket-lang.org/reference/threads.html

(define-record thread (eng id s-to-k? suspended?))
(define-record scheduler (queue idle?))

(define (global-queue)
  (scheduler-queue global-scheduler))

(define thread-id-counter 0)

(define global-scheduler (make-scheduler '() #t))

(define (busy-loop)
  (cond
   [(empty? (scheduler-queue global-scheduler)) ;; nothing to schedule
    (engine-block)
    (busy-loop)]
   [(scheduler-idle? global-scheduler) ;; something to schedule
    (let ([newt (car (scheduler-queue global-scheduler))])
      (set-scheduler-idle?! #f)
      ((thread-eng newt) 50 completed (expired newt)))]
   [else ;; something already running
    (engine-block)
    (busy-loop)]))

(define (sched-finished fuel values)
  (printf "This should never happen!\n"))

(define (sched-complete eng)
  (eng 500 sched-finished sched-complete))

;; start scheduler.
((make-engine busy-loop) 500 sched-finished sched-complete)

;; add something to schedulers queue
(define (enqueue t)
  (set-scheduler-queue! global-scheduler
			(append (scheduler-queue global-scheduler)
				(list t))))

(define (thread-completed fuel values)
  (set-scheduler-queue! global-scheduler ;; remove self from queue.
			(cdr (scheduler-queue global-scheduler)))
  (set-scheduler-idle?! global-scheduler #t)
  values)

(define (thread-expired t)
  (lambda (eng)
    (set-thread-eng! t eng)
    (enqueue t)))

;; 11.1.1 creating threads
#|
  Calls thunk with no arguments in a new thread of control. 
  The thread procedure returns immediately with a thread 
  descriptor value. When the invocation of thunk returns, 
  the thread created to invoke thunk terminates.
|#
(define (thread thunk)
  (let* ([tid (begin (set! thread-id-counter (+ 1 thread-id-counter))
		     thread-id-counter)]
	 [t (make-thread (make-engine thunk) tid #f #f)])
    (enqueue t)))

;; creates a thread.... with a special property
(define (thread/suspend-to-kill thunk)
  (let* ([tid (begin (set! thread-id-counter (+ 1 thread-id-counter))
		     thread-id-counter)]
	 [t (make-thread (make-engine thunk) tid #t #f)])
    (enqueue t)))

;; what if thread has terminated?
(define (thread? v)
  (and (> v 0) (<= v thread-id-counter)))

(define (current-thread)
  ;; what if there is no current thread?
  (let ([q (scheduler-queue global-scheduler)])
    (if (empty? q)
	#f
	(thread-id (car q)))))

(define call-in-nested-thread  ;; thunk [cust]
  (case-lambda 
   [(thunk) 
    ]
   [(thunk cust) 
    ]))

;; 11.1.2
(define (thread-suspend thd)
  )

(define thread-resume ;;thd [benefactor] 
  (case-lambda
   [(thd)
    ]
   [(thd benefactor)
    ]))

(define (kill-thread thd)
  )

(define break-thread ;; thd [kind]
  (case-lambda
   [(thd)
    ]
   [(thd kind)
    ]))

(define sleep ;; [secs]
  (case-lambda
   [()
    ]
   [(secs)
    ]))

(define (thread-running? thd)
  (if (thread-dead? thd)
      #f
      (let loop ([lst (global-queue)]) ;; thread-suspended?
	(cond
	 [(empty? lst)
	  #f]
	 [(eqv? (thread-id (car lst)) thd)
	  (not (thread-suspended? (car lst)))]
	 [else
	  (loop (cdr lst))]))))

(define (thread-dead? thd)
  (let loop ([lst (global-queue)])
    (cond
     [(empty? lst)
      #t]
     [(eqv? (thread-id (car lst)) thd)
      #f]
     [else
      (loop (cdr lst))])))

;; 11.1.2 synchronizing
(define (thread-wait thd)
  )

(define (thread-dead-evt thd)
  )

(define (thread-resume-evt thd)
  )

(define (thread-suspend-evt thd)
  )

;; 11.1.4 thread mailboxes
(define thread-send ;; thd v [fail-thunk]
  (case-lambda
   [(thd v)
    ]
   [(thd v fail-thunk)
    ]))

(define (thread-receive)
  )

(define (thread-try-receive)
  )

(define (thread-receive-evt)
  )

(define (thread-rewind-receive lst)
  )

