
(define-record thread-internal (eng id s-to-k? suspended?))
(define-record scheduler (queue idle?))

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
    (set-thread-internal-eng! t eng)
    (enqueue t)))

(define (busy-loop)
  (cond
   [(atom? (scheduler-queue global-scheduler)) ;; nothing to schedule
    (engine-block)
    (busy-loop)]
   [(scheduler-idle? global-scheduler) ;; something to schedule
    (let ([newt (car (scheduler-queue global-scheduler))])
      (set-scheduler-idle?! global-scheduler #f)
      ((thread-internal-eng newt) 50 thread-completed (thread-expired newt)))]
   [else ;; something already running
    (engine-block)
    (busy-loop)]))

(define (sched-finished fuel values)
  (printf "This should never happen!\n"))

(define (sched-complete eng)
  (eng 500 sched-finished sched-complete))

;; 11.1.1 creating threads
#|
  Calls thunk with no arguments in a new thread of control. 
  The thread procedure returns immediately with a thread 
  descriptor value. When the invocation of thunk returns, 
  the thread created to invoke thunk terminates.
|#

;; return t instead of tid.

(define (thread thunk)
  (let* ([tid (begin (set! thread-id-counter (+ 1 thread-id-counter))
		     thread-id-counter)]
	 [t (make-thread-internal (make-engine thunk) tid #f #f)])
    (enqueue t)
    t))

;; creates a thread.... with a special property
(define (thread/suspend-to-kill thunk)
  (let* ([tid (begin (set! thread-id-counter (+ 1 thread-id-counter))
		     thread-id-counter)]
	 [t (make-thread-internal (make-engine thunk) tid #t #f)])
    (enqueue t)))

;; what if thread has terminated?
(define (thread? v)
  (thread-internal? v))

(define (current-thread)
  ;; what if there is no current thread?
  (let ([q (scheduler-queue global-scheduler)])
    (if (atom? q)
	#f
	(car q))))

;; start scheduler.
;;((make-engine busy-loop) 10 sched-finished sched-complete)
