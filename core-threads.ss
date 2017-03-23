;; https://docs.racket-lang.org/reference/threads.html

(define-record thread (eng id suspend?))
(define-record scheduler (queue))

(define thread-id-counter 0)

(define global-scheduler (make-scheduler '()))

;; need a real thread to run scheduler.
(define (enqueue t)
  (if (empty? (scheduler-queue global-scheduler))
      (begin
	(set-scheduler-queue! global-scheduler (list t))
	((thread-eng t) 50 completed (expired t)))
      (set-scheduler-queue! global-scheduler
			    (append (scheduler-queue global-scheduler)
				    (list t)))))

(define (completed fuel values)
  (set-scheduler-queue! global-scheduler
			(cdr (scheduler-queue global-scheduler)))
  (if (not (empty? (scheduler-queue global-scheduler)))
      (let ([newt (car (scheduler-queue global-scheduler))])
	((thread-eng newt) 50 completed (expired newt)))
      (void)))

(define (expired t)
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
	 [t (make-thread (make-engine thunk) tid #f)])
    (enqueue t)))

;; creates a thread.... with a special property
(define (thread/suspend-to-kill thunk)
  (let* ([tid (begin (set! thread-id-counter (+ 1 thread-id-counter))
		     thread-id-counter)]
	 [t (make-thread (make-engine thunk) tid #t)])
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
  )

(define (thread-dead? thd)
  )

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

