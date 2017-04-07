
(define-record thread-internal (thunk eng stok? suspended? blocked? mailbox))

(define (create-thread thunk stok?)
  (make-thread-internal thunk #f stok? #f #f '()))

(define-record scheduler (queue bqueue mtx))

(define (global-queue)
  (scheduler-queue global-scheduler))
(define (blocked-queue)
  (scheduler-bqueue global-scheduler))

(define to-become-engines '())
(define (enqueue-tbe thd)
  (set! to-become-engines (append to-become-engines (list thd))))
(define (dequeue-tbe)
  (let ([thd (car to-become-engines)])
    (set! to-become-engines (cdr to-become-engines))
    thd))

(define global-scheduler (make-scheduler '() '() (make-lock)))
(define (enqueue thd)
  (set-scheduler-queue! global-scheduler (append (global-queue)
						 (list thd))))
(define (dequeue)
  (let ([thd (car (global-queue))])
    (set-scheduler-queue! global-scheduler (cdr (global-queue)))
    thd))

;; initializes the engines in the newly created threads. and enqueues them.
(define (create-new-engines)
  (let loop ([i 10]) ;; just so we don't get stuck forver making engines and never schedule anything
    (cond
     [(or (<= i 0) (atom? to-become-engines))
      (void)]
     [else
      (let ([thd (dequeue-tbe)])
	(set-thread-internal-eng! thd (make-engine (thread-internal-thunk thd)))
	(enqueue thd)
	(loop (- i 1)))])))

(define (run-sched)
  ;; check if there are new threads to be created.
  (cond
   [(not (atom? to-become-engines))
    (create-new-engines)])
  ;; check that there is now something to schedule?
  (cond
   [(and (atom? (global-queue))  ;; if nothing to schedule & nothing blocked. die.
	 (atom? (blocked-queue)))
    (void)]
   [else
    (let ([thd (dequeue)])
      ((thread-internal-eng thd) 50 (thread-completed thd)
       (thread-expired thd)))
      ;; what about here? 
    ])) ;; deal with things in the blocked-queue...

(define (thread-completed thd)
  (lambda (fuel values)
    (run-sched))) ;; enough?

(define (thread-expired thd)
  (lambda (eng)
    (set-thread-internal-eng! thd eng)
    (enqueue thd)
    (run-sched))) ;; enough?

;; api functions

;; special thread function only for the first thread.
(define (start-program thunk)
  (let ([thd (make-thread-internal thunk 
				   (make-engine thunk) 
				   #f #f #f '())])
    (enqueue thd)
    (run-sched)))

;; need to create threads differently.  store engine after creating thread.

(define (thread thunk)
  (let ([thd (create-thread thunk #f)])
    (enqueue-tbe thd)
    thd))

(define (thread/suspend-to-kill thunk)
  (let ([thd (create-thread thunk #t)])
    (enqueue-tbe thd)
    thd))

(define (thread? v)
  (thread-internal? v))

;; Not sure of best approach here.

(define (current-thread)
  (void))

(define (kill-thread thd)
  (void))

(define (thread-resume thd)
  (void))

(define thread-send
  (case-lambda
   [(thd msg)
    (void)]
   [(thd msg fail)
    (void)]))

(define (thread-suspend thd)
  (void))






 
  
