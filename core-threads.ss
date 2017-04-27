
(trace-define cdr_ cdr)
(trace-define car_ car)

(define in-engine? #f)

(define largest-id -1)

(define global-counter 0)

(define (get-id) ;; should be atomic so two threads dont get same id.
  (atomically
   (let ([id global-counter])
     (set! global-counter (+ 1 id))
     id)))

(define-record thread-internal (id thunk eng stok? suspended? terminated? blocked? wakeup mailbox))
(define-record scheduler (queue bqueue current mtx))

;; use mutable pairs instead of list.  maintain pointer to front and end to list so it is constant.
;; move pointer not threads to not do extra allocation
;; what about vectors?
;; todo.

;; point not a box at linklet
;; call eval on benchmark ... ala expander-demo.ss
;; LINKLET_RACKET env variable.  not-a-box.


(define (global-queue)
  (scheduler-queue global-scheduler))
(define (blocked-queue)
  (scheduler-bqueue global-scheduler))

(define to-become-engines '())
(define (enqueue-tbe thd)
  (atomically
   (set! to-become-engines (append to-become-engines (list thd)))))

;; doesn't need to be atomic only called from non-engine code 
(define (dequeue-tbe)
  (let ([thd (car to-become-engines)])
    (set! to-become-engines (cdr to-become-engines))
    thd))

(define (create-thread thunk stok?)
  (make-thread-internal (get-id) thunk #f stok? #f #f #f #f '()))

;; called atomically from thread-send
(define (enqueue-mail thd msg)
  (set-thread-internal-mailbox! thd (append (thread-internal-mailbox thd) (list msg))))


(define (dequeue-mail thd)
  (atomically
   (let ([mbx (thread-internal-mailbox thd)])
     (let ([mail (car mbx)])
       (let ([cd (cdr mbx)])
	 (set-thread-internal-mailbox! thd cd)
	 mail)))))

;; doesn't need to be atomic only called from non-engine code
(define (blocked-enqueue thd)
  (set-scheduler-bqueue! global-scheduler (append (blocked-queue)
						  (list thd))))

;; doesn't need to be atomic only called from non-engine code
(define (enqueue-unblocked-threads)
  (set-scheduler-bqueue! 
   global-scheduler 
   
   (let loop ([q (blocked-queue)])
     (cond
      [(atom? q)
       q]
      [(not (atom? (thread-internal-mailbox (car q)))) ;; can unblock
       (set-thread-internal-blocked?! (car q) #f) 
       ;; not removed from unblocked queue here. so fails test even though it is ok.
       (enqueue (car q))
       (loop (cdr q))]  ;; thread 
      [else
       (let ([cd (cdr q)])
	 (cons (car q)
	       (loop cd)))]))))

;; implement own atomicity based on matthews.

;; this function is called from inside an engine.

(define global-scheduler (make-scheduler '() '() #f (make-lock)))

;; called from thread-resume.
(define (enqueue thd)
  (cond
   [(in-queue? thd (global-queue))
    (errorf 'enqueue "Thread is already in queue: ~a\n" thd)]
   [(in-queue? thd (blocked-queue))
    (void)]
   ;;  (errorf 'enqueue "Thread is already in blocked-queue:  ~a\n" thd)]
   [(in-queue? thd to-become-engines)
    (errorf 'enqueue "Thread is already in tbe queue: ~a\n" thd)])
  
  (set-scheduler-queue! global-scheduler (append (global-queue)
						 (list thd))))
  
;; only called from non-engine code
(define (dequeue)
  (let ([thd (car (global-queue))])
    (set-scheduler-queue! global-scheduler (cdr (global-queue)))
    thd))

(define (in-queue? thd q)
  (cond
   [(atom? q)
    #f]
   [(eq? thd (car q))
    #t]
   [else
    (in-queue? thd (cdr q))]))

;; initializes the engines in the newly created threads. and enqueues them.
;; only called from non-engine code
(define (create-new-engines)
  (let loop ([i 50]) ;; just so we don't get stuck forever making engines and never schedule anything
    (cond
     [(or (<= i 0) (atom? to-become-engines))
      (void)]
     [else
      (let ([thd (dequeue-tbe)])
	(cond
	 [(in-queue? thd (global-queue))
	  (errorf 'cne "Thread is already in queue: ~a\n" thd)]
	 [(in-queue? thd (blocked-queue))
	  (errorf 'cne "Thread is already in blocked-queue:  ~a\n" thd)]
	 [(in-queue? thd to-become-engines)
	  (errorf 'cne "Thread is already in tbe queue: ~a\n" thd)])
	
	(set-thread-internal-eng! thd (make-engine (thread-internal-thunk thd)))

	(if (<= (thread-internal-id thd) largest-id)
	    (errorf 'cne "Id is smaller: ~a ~a\n\n" largest-id thd))

	(set! largest-id (thread-internal-id thd))
	(enqueue thd)
	(loop (- i 1)))])))

(define (in-queue-twice? thd q)
  (cond
   [(atom? q)
    0]
   [(eq? thd (car q))
    (+ 1 (in-queue-twice? thd (cdr q)))]
   [else
    (in-queue-twice? thd (cdr q))]))

(define (run-sched)
  (cond
   [(not (atom? to-become-engines))
    (create-new-engines)])
  ;; check if things have unblocked
  (enqueue-unblocked-threads)
  
  (cond
   [(atom? (global-queue))  ;; if nothing to schedule die
    (void)]
   [else
    (let loop ()
      (cond
       [(atom? (global-queue))
	(run-sched)] ;; if nothing to schedule check if there are new threads or things are unblocked
       [else
	(let ([thd (dequeue)])
	  (cond
	   [(or (thread-internal-suspended? thd)
		(thread-internal-terminated? thd))
	    (loop)] ;; try again
	   [else
	    (set-scheduler-current! global-scheduler thd)
	    ((thread-internal-eng thd) 100 (thread-completed thd)
	     (thread-expired thd))]))]))]))

(define (thread-completed thd) ;; check if in atomic?
  (lambda (fuel values)
    (if (not (zero? (current-atomic)))
	(errorf 'thread-completed "Error: thread terminated while running atomically ~a\n" thd))
    (set-thread-internal-terminated?! thd #t)
    (run-sched)))

;; matthew calls engine-block in atomic section. how does that work?

(define (thread-expired thd)
  (lambda (eng)
    (set-thread-internal-eng! thd eng)
    (cond
     [(not (zero? (current-atomic))) ;; run engine again because running atomically
      ((thread-internal-eng thd) 100 (thread-completed thd)
       (thread-expired thd))]
     [(thread-internal-blocked? thd) ;; just assume engine block never called in atomic section
      (blocked-enqueue thd)]
     [(and (not (thread-internal-suspended? thd))
	   (not (thread-internal-terminated? thd)))
      (enqueue thd)])

    (run-sched)))

;; api functions

;; special thread function only for the first thread.
(define (start-program thunk)
  (let ([thd (make-thread-internal (get-id) thunk 
				   (make-engine thunk) 
				   #f #f #f #f #f '())])
    (enqueue thd)
    (run-sched)))

(define (thread thunk)
  (let ([thd (create-thread thunk #f)])
    (enqueue-tbe thd) ;; this is running in an engine..... can be pre-empted.
    thd))

(define (thread/suspend-to-kill thunk)
  (let ([thd (create-thread thunk #t)])
    (enqueue-tbe thd)
    thd))

(define (thread? v)
  (thread-internal? v))

;; if this function is called there is a thread running
(define (current-thread)
  (scheduler-current global-scheduler))

(define (thread-suspend thd)
  (cond
   [(or (thread-internal-suspended? thd) (thread-internal-terminated? thd))
    (void)]
   [else
    (set-thread-internal-suspended?! thd #t)
    (engine-block)]))

(define thread-resume
  (case-lambda
   [(thd)
    (atomically ;; atomic because we call enqueue...
     (cond
      [(or (thread-internal-terminated? thd)
	   (not (thread-internal-suspended? thd))) ;; not necessary to check
       (void)]
      [(in-queue? thd (global-queue))
       (set-thread-internal-suspended?! thd #f)]
      [else
       (set-thread-internal-suspended?! thd #f)
       (enqueue thd)]))
    ]
   [(thd benefactor)
    (atomically
     (cond
      [(or (thread-internal-terminated? thd)
	   (not (thread-internal-suspended? thd))) ;; not necessary to check
       (void)]
      [(in-queue? thd (global-queue))
       (set-thread-internal-suspended?! thd #f)]
      [else
       (set-thread-internal-suspended?! thd #f)
       (enqueue thd)]))
    ]))

;; terminating the main thread exits the application. todo
(define (kill-thread thd)
  (cond
   [(not (thread-internal-terminated? thd))
    (if (thread-internal-stok? thd)
	(set-thread-internal-suspended?! thd #t)
	(set-thread-internal-terminated?! thd #t))])
  (void))

(define (thread-running? thd)
  (and (not (thread-internal-terminated? thd))
       (not (thread-internal-suspended? thd))))

(define (thread-dead? thd)
  (thread-internal-terminated? thd))

;; mailboxes

(define thread-send
  (case-lambda
   [(thd msg)
    (atomically
     (cond
      [(thread-running? thd)
       (enqueue-mail thd msg)]
      [else
       ((lambda () (raise-mismatch-error 'thread-send "Could not send message ~v\n" msg)))]))] ;;error
   [(thd msg fail)
    (atomically
     (cond
      [(thread-running? thd)
       (enqueue-mail thd msg)]
      [(not fail)
       #f]
      [else
       (fail)]))]))

;; if this code runs the thread is not blocked. so does not behave like thread-send
(define (thread-rewind-receive lst)
  (let ([ct (current-thread)])
    (atomically 
     (set-thread-internal-mailbox! ct (append (reverse lst) (thread-internal-mailbox ct))))))

;; does this need to be atomic? no?
(define (thread-receive)
  (let ([ct (current-thread)])
    (cond
     [(atom? (thread-internal-mailbox ct)) ;; mailbox is empty so blockd
      (set-thread-internal-blocked?! ct #t)
      (engine-block)])
    (dequeue-mail ct))) ;; dequeue-mail is atomic

(define (thread-try-receive)
  (let ([ct (current-thread)])
    (cond
     [(atom? (thread-internal-mailbox ct))
      #f]
     [else
      (dequeue-mail ct)])))

(define sleep
  (case-lambda
   [()
    (void)
    ]
   [(secs)
    (void)]))

;; 


 
  
