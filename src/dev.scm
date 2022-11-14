;; dev.scm -- "dev mode" (runs main as thread for REPL development)
(import srfi-18
        spiffy
        (chicken process-context))

(include "main.scm")

(access-log "/tmp/rsvp-access.log")
(error-log "/tmp/rsvp-error.log")

(print "Logging access to " (access-log))
(print "Logging error to " (error-log))

(define thread
  (begin
    (thread-start!
      (make-thread (lambda () (main '()))))))
