;; dev.scm -- "dev mode" (runs main as thread for REPL development)
(import srfi-18
        (chicken process-context))

(include "main.scm")

(define thread
  (begin
    (thread-start!
      (make-thread (lambda () (main '()))))))
