;; stolen from mflatt

(define atomic 0)
(define current-atomic
  (case-lambda
    [() atomic]
    [(v) (set! atomic v)]))

(define-syntax atomically
  (syntax-rules ()
    [(atomically expr ...)
     (begin
       (start-atomic)
 ;;      (fprintf (current-error-port) "Entered atomic\n\n")
       (begin0
           (let () expr ...)
	   (end-atomic)))
;;	 (fprintf (current-error-port) "Exit atomic\n\n")))
     ]))

(define (start-atomic)
  (current-atomic (add1 (current-atomic))))

(define (end-atomic)
  (current-atomic (sub1 (current-atomic))))
