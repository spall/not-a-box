#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "struct-type-info.rkt"
         "simple.rkt")

(provide find-definitions
         lambda?)

;; Record top-level functions and structure types, and returns
;;  (values knowns struct-type-info-or-#f)
(define (find-definitions v prim-knowns knowns imports mutated)
  (match v
    [`(define-values (,id) ,rhs)
     (values
      (cond
       [(lambda? rhs)
        (hash-set knowns (unwrap id) a-known-procedure)]
       [(simple? rhs prim-knowns knowns imports mutated)
        (hash-set knowns (unwrap id) a-known-unknown)]
       [else knowns])
      #f)]
    [`(define-values (,struct:s ,make-s ,s? ,acc/muts ...) ; pattern from `struct` or `define-struct`
       (let-values (((,struct: ,make ,? ,-ref ,-set!) ,rhs))
         (values ,struct:2
                 ,make2
                 ,?2
                 ,make-acc/muts ...)))
     (define info (and (wrap-eq? struct: struct:2)
                       (wrap-eq? make make2)
                       (wrap-eq? ? ?2)
                       (make-struct-type-info rhs prim-knowns knowns imports mutated)))
     (cond
      [info
       (define type (gensym (symbol->string (unwrap make-s))))
       (let* ([knowns (hash-set knowns
                                (unwrap make-s)
                                (if (struct-type-info-pure-constructor? info)
                                    (known-constructor type (struct-type-info-field-count info))
                                    a-known-procedure))]
              [knowns (hash-set knowns
                                (unwrap s?)
                                (known-predicate type))]
              [knowns
               (for/fold ([knowns knowns]) ([id (in-list acc/muts)]
                                            [maker (in-list make-acc/muts)])
                 (cond
                  [(wrap-eq? (wrap-car maker) -ref)
                   (hash-set knowns (unwrap id) (known-accessor type))]
                  [else
                   (hash-set knowns (unwrap id) (known-mutator type))]))])
         (values (hash-set knowns (unwrap struct:s) (known-struct-type type (struct-type-info-field-count info)))
                 info))]
      [else (values knowns #f)])]
    [`(define-values (,struct:s ,make-s ,s? ,s-ref ,s-set!) ,rhs) ; direct use of `make-struct-type`
     (define info (make-struct-type-info rhs prim-knowns knowns imports mutated))
     (cond
      [info
       (define type (gensym (symbol->string (unwrap make-s))))
       (values
        (let* ([knowns (hash-set knowns
                                 (unwrap make-s)
                                 (if (struct-type-info-pure-constructor? info)
                                     (known-constructor type (struct-type-info-field-count info))
                                     a-known-procedure))]
               [knowns (hash-set knowns
                                 (unwrap s?)
                                 (known-predicate type))])
          ;; For now, we don't try to track the position-consuming accessor or mutator
          (hash-set knowns (unwrap struct:s) (known-struct-type type (struct-type-info-field-count info))))
        info)]
      [else (values knowns #f)])]
    [`(define-values (,prop:s ,s? ,s-ref)
       (make-struct-type-property ,_ . ,rest))
     (define type (gensym (symbol->string prop:s)))
     (values
      (let* ([knowns (hash-set knowns (unwrap s-ref) (known-accessor type))]
             [knowns (hash-set knowns (unwrap s?) (known-predicate type))])
        ;; Check whether the property type has an immediate (or no) guard:
        (cond
         [(or (null? (unwrap rest))
              (and (not (wrap-car rest))
                   (null? (unwrap (wrap-cdr rest)))))
          (hash-set knowns (unwrap prop:s) a-known-struct-type-property/immediate-guard)]
         [else knowns]))
      #f)]
    [`,_ (values knowns #f)]))

;; ----------------------------------------

;; Recognize forms that produce plain procedures
(define (lambda? v)
  (match v
    [`(lambda . ,_) #t]
    [`(case-lambda . ,_) #t]
    [`(let-values ([(,id) ,rhs]) ,body) (or (and (wrap-eq? id body) (lambda? rhs))
                                            (lambda? body))]
    [`(letrec-values ([(,id) ,rhs]) ,body) (or (and (wrap-eq? id body) (lambda? rhs))
                                               (lambda? body))]
    [`(let-values ,_ ,body) (lambda? body)]
    [`(letrec-values ,_ ,body) (lambda? body)]
    [`,_ #f]))
