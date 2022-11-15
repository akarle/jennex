#!/usr/local/bin/chicken-csi -ss
;; main.scm -- configure and start the Spiffy web server
(import 
  srfi-18
  spiffy
  spiffy-uri-match
  (chicken process-context)
  (chicken format))

(include "handler.scm")  ;; contains 'routes'

(access-log (current-output-port))
(error-log (current-error-port))
(root-path "../")

;; Spiffy recommends using vhost-map to assign dynamic routes
;; (even if we aren't using the vhost portion of it)
(vhost-map `((".*" . ,(uri-match/spiffy routes))))

(define (main args)
  (print (format "Starting up! Listening on port ~A..." (server-port)))
  (start-server))
