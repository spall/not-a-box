
(define-record thread-internal (eng id s-to-k? suspended? terminated? completed expired mailbox))
;; https://docs.racket-lang.org/reference/threads.html
(define-record scheduler (queue idle? mtx))

(define (global-queue)
  (scheduler-queue global-scheduler))

(define (acquire-sched-lock)
  (lock-acquire (scheduler-mtx global-scheduler)))

(define (release-sched-lock)
  (lock-release (scheduler-mtx global-scheduler)))

(define thread-id-counter 0)

(define global-scheduler (make-scheduler '() #t (make-lock)))

;; add something to schedulers queue
(define (enqueue t)
  (acquire-sched-lock)
  (set-scheduler-queue! global-scheduler
			(append (global-queue)
				(list t)))
  (release-sched-lock))

(define (thread-completed t)
  (lambda(fuel values)
    (acquire-sched-lock)
    (set-scheduler-queue! global-scheduler ;; remove self from queue. 
			  (cdr (global-queue)))
    (set-scheduler-idle?! global-scheduler #t)
    (set-thread-internal-terminated?! t #t)
    (release-sched-lock)
    values))

(define (thread-expired t)
  (lambda (eng)
    (acquire-sched-lock)
    (set-scheduler-queue! global-scheduler ;; remove self from queue. 
			  (cdr (global-queue)))
    (set-scheduler-idle?! global-scheduler #t)
    (release-sched-lock)
    (set-thread-internal-eng! t eng)
    (enqueue t)))

(define (busy-loop)
  (acquire-sched-lock)
  (cond
   [(atom? (global-queue)) ;; nothing to schedule
    (release-sched-lock)
    (busy-loop)]
   [(scheduler-idle? global-scheduler) ;; something to schedule
    (let ([newt (car (global-queue))])
      (set-scheduler-idle?! global-scheduler #f)
      (release-sched-lock)
      ((thread-internal-eng newt) 50 ((thread-internal-completed newt) newt)
       ((thread-internal-expired newt) newt))
      (busy-loop))]
   [else ;; something already running
    (release-sched-lock)
    (busy-loop)]))

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
	 [t (make-thread-internal (make-engine thunk) tid #f #f #f
				  thread-completed thread-expired '())])
    (enqueue t)
    t))

;; creates a thread.... with a special property
(define (thread/suspend-to-kill thunk)
  (let* ([tid (begin (set! thread-id-counter (+ 1 thread-id-counter))
		     thread-id-counter)]
	 [t (make-thread-internal (make-engine thunk) tid #t #f #f
				  thread-completed thread-expired '())])
    (enqueue t)
    t))

(define (thread? v)
  (thread-internal? v))

(define (current-thread)
  ;; what if there is no current thread?
  (acquire-sched-lock)
  (let ([q (global-queue)])
    (release-sched-lock)
    (if (atom? q)
	#f
	(car q))))

(define (is-front? thd q)
  (cond
   [(atom? q)
    #f]
   [(eq? thd (car q))
    #t]
   [else 
    #f]))

(define (remove-from-queue thd q)
  (cond
   [(atom? q)
    q]
   [(eq? thd (car q))
    q]
   [else
    (remove-from-queue thd (cdr q))]))

(define (thread-suspend thd)
  (cond
   [(and (not (thread-internal-terminated? thd))
	 (not (thread-internal-suspended? thd)))
    (set-thread-internal-suspended?! thd #t)
    (acquire-sched-lock)
    (let ([q (global-queue)])
      (cond
       [(not (is-front? thd q))
	(set-scheduler-queue! global-scheduler (remove-from-queue thd q))])
      (release-sched-lock))]))

(define (thread-running? thd)
  (and (not (thread-internal-terminated? thd))
       (not (thread-internal-suspended? thd))))

(define (thread-dead? thd)
  (thread-internal-terminated? thd))

(define thread-resume
  (case-lambda
   [(thd)
    (cond
     [(and (not (thread-internal-terminated? thd))
	   (thread-internal-suspended? thd))
      (begin 
	(set-thread-internal-suspended?! thd #f)
	(enqueue thd))])]
   [(thd benefactor) ;; ignore benefactor
    (cond
     [(and (not (thread-internal-terminated? thd))
	   (thread-internal-suspended? thd))
      (begin
       (set-thread-internal-suspended?! thd #f)
       (enqueue thd))])
    ]))

;; terminate the main thread?
(define (kill-thread thd)
  (cond
   [(thread-internal-terminated? thd)
    (void)]
   [(thread-internal-s-to-k? thd)
    (thread-suspend thd)]
   [else
    (set-thread-internal-terminated?! thd #t)])) ;; do i need to remove from queue?

;; MAILBOXES

(define thread-send
  (case-lambda
   [(thd msg)
    (if (not (thread-running? thd))
	((lambda () (raise-mismatch-error 'thread-send "Could not send message ~v\n" msg)))
	(set-thread-internal-mailbox! thd (append (thread-internal-mailbox thd) (list msg))))
    ]
   [(thd msg fail)
    (if (thread-running? thd)
	(set-thread-internal-mailbox! thd (append (thread-internal-mailbox thd) (list msg)))
	(if (not fail)
	    #f
	    (fail)))
    ]))

;; block WHOLE REAL THREAD NOT JUST CURRENT "THREAD", probably bad.
(define (thread-receive)
  ;; what if there is no current thread...
  (let ([ct (current-thread)])
    (if (not ct)
	#f
	(let loop ()
	  (if (atom? (thread-internal-mailbox ct))
	      (loop)
	      (let* ([mailbox (thread-internal-mailbox ct)]
		     [mail (car mailbox)])
		(set-thread-internal-mailbox! ct (cdr mailbox))
		mail))))))

;; need ot change when locks are added
(define (thread-try-receive)
  ;; what if there is no current thread...
  (let ([ct (current-thread)])
    (cond
     [(not ct) #f]
     [(atom? (thread-internal-mailbox ct))
      #f]
     [else
      (let* ([mailbox (thread-internal-mailbox ct)]
	     [mail (car mailbox)])
	(set-thread-internal-mailbox! ct (cdr mailbox))
	mail)])))

(define (thread-rewind-receive lst)
  ;; what if there is no current thread...
  (let ([ct (current-thread)])
    (if (not ct)
	#f
	(set-thread-internal-mailbox! ct (fold-left (lambda (accu x)
						      (cons x accu))
						    (thread-internal-mailbox ct)
						    lst)))))

;; start scheduler.
  (fork-thread busy-loop)


;; what about blocking for a mailbox? what was the behavior sam asked about?
