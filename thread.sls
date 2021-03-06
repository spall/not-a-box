(library (thread)
  (export)
  (import (chezpart)
          (rename (only (chezscheme)
                        sleep
                        printf)
                  [sleep chez:sleep])
          (rename (core)
                  ;; These are extracted via `#%linklet`:
                  [make-engine core:make-engine]
                  [engine-block core:engine-block]
                  [engine-return core:engine-return]
                  [root-continuation-prompt-tag core:root-continuation-prompt-tag]))
  
  (define call-with-escape-continuation call-with-current-continuation)

  (define (sleep secs)
    (define isecs (inexact->exact (floor secs)))
    (chez:sleep (make-time 'time-duration
                           (inexact->exact (floor (* (- secs isecs) 1e9)))
                           isecs)))

  (define (primitive-table key)
    (case key
      [(|#%engine|) (hash
                     'make-engine core:make-engine
                     'engine-block core:engine-block
                     'engine-return core:engine-return
                     'root-continuation-prompt-tag core:root-continuation-prompt-tag)]
      [else #f]))

  (include "thread.scm"))
