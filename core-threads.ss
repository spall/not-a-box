
(define-record thread-internal (eng id s-to-k? suspended? completed expired))
;; https://docs.racket-lang.org/reference/threads.html
(define-record scheduler (queue idle?))
;; i think i need a lock already. since the busy loop is in 1 thread and things are enqueued from
;; the main thread

(define (global-queue)
  (scheduler-queue global-scheduler))

(define thread-id-counter 0)

(define global-scheduler (make-scheduler '() #t))

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
    (set-scheduler-queue! global-scheduler ;; remove self from queue. 
			  (cdr (scheduler-queue global-scheduler)))
    (set-scheduler-idle?! global-scheduler #t)
    (set-thread-internal-eng! t eng)
    (enqueue t)))

(define (busy-loop)
  (cond
   [(atom? (scheduler-queue global-scheduler)) ;; nothing to schedule
    (busy-loop)]
   [(scheduler-idle? global-scheduler) ;; something to schedule
    (let ([newt (car (scheduler-queue global-scheduler))])
      (set-scheduler-idle?! global-scheduler #f)
      ((thread-internal-eng newt) 50 (thread-internal-completed newt) 
       ((thread-internal-expired newt) newt))
      (busy-loop))]
   [else ;; something already running
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
	 [t (make-thread-internal (make-engine thunk) tid #f #f
				  thread-completed thread-expired)])
    (enqueue t)
    t))

;; creates a thread.... with a special property
(define (thread/suspend-to-kill thunk)
  (let* ([tid (begin (set! thread-id-counter (+ 1 thread-id-counter))
		     thread-id-counter)]
	 [t (make-thread-internal (make-engine thunk) tid #t #f
				  thread-completed thread-expired)])
    (enqueue t)
    t))

(define (thread? v)
  (thread-internal? v))

(define (current-thread)
  ;; what if there is no current thread?
  (let ([q (scheduler-queue global-scheduler)])
    (if (atom? q)
	#f
	(car q))))

;; start scheduler.
(fork-thread busy-loop)

