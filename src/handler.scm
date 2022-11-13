;; handler.scm -- define the routes for Spiffy
(import
  spiffy
  uri-common
  intarweb
  sxml-serializer
  sql-null
  (chicken string))

(load "db.scm")

(define routes
  ;; See uri-match for format: http://wiki.call-cc.org/eggref/5/uri-match#routes-format
  ;;
  ;; NOTE: wrap the route handlers in lambdas so that the REPL picks up
  ;; changes on re-definition.
  `(((/ "") (GET ,(lambda (c) (route-get-index c))))
    ((/ "rsvp")
     (GET ,(lambda (c) (route-get-rsvp c)))
     (POST ,(lambda (c) (route-post-rsvp c))))))

(define (send-sxml sxml)
  (send-response status: 'ok body: (serialize-sxml sxml)))

(define (template-page sxml)
  `(html
     (head (title "Alex and Jennie's Wedding")
           (meta (@ (charset "utf-8")))
           (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
            (link (@ (rel "icon") (type "image/png") (sizes "32x32") (href "/static/favicon-32x32.png")))
            (link (@ (rel "icon") (type "image/png") (sizes "16x16") (href "/static/favicon-16x16.png")))
           (link (@ (rel "stylesheet") (href "/style.css"))))
     (body
       (h1 (@ (class "index-title")) "Alex " (small "&") " Jennie")
       (p (@ (class "subtitle")) "Tie the Knot")
       (nav 
         (a (@ (href "https://jennex.org")) "Home")
         (a (@ (href "https://jennex.org/story.html")) "Our Story")
         (a (@ (href "https://jennex.org/event.html")) "Event")
         (a (@ (href "https://jennex.org/travel.html")) "Travel")
         (a (@ (href "https://jennex.org/registry.html")) "Registry"))
       ,@sxml
       (footer "Copyright 2022, Alex Karle (" (a (@ (href "https://jennex.org/license.html")) "License") ")"))))


(define (route-get-index c)
  (send-sxml
    (template-page
      '((h2 "RSVP")
        (p "Thanks for RSVP'ing! To start, please lookup the "
           "name on your invitation.")
        (form (@ (action "/rsvp"))
          (label "Name:"
                 (input (@ (name "rsvp-name"))))
          (button "Lookup"))
        (p "Please let us know by DATE whether you can make it!")))))

(define (guest-to-form g)
  ;; Given a guest record, generate a SXML form for their settings
  ;; TODO: expand plus-1's
  (define (get-going-attrs val)
    `((name ,(conc (number->string (guest-id g)) "__going"))
      (value ,val)
      (type "radio")
      (required "true")
      ,@(let ((going (guest-going g)))
	 (cond ((equal? val "no") (if (eq? going 0) '((checked)) '()))
	       ((equal? val "yes") (if (eq? going 1) '((checked)) '()))
	       ((equal? val "null") (if (sql-null? going) '((checked)) '()))
	       (else (error "Bad val"))))))

  (define (get-meal-attrs val)
    `((name ,(conc (number->string (guest-id g)) "__meal_choice"))
      (value ,val)
      (type "radio")
      (required "true")
      ,@(if (equal? (guest-meal-choice g) val)
	      '((checked))
	      '())))

  ;; TODO: add notes section for allergies, etc
  `((div (@ (class "guest"))
	 (h4 ,(guest-name g))
	 (fieldset
	  (legend "Will You be Attending?")
	  (label (input (@ ,@(get-going-attrs "yes"))) "Yes!")
	  (br)
	  (label (input (@ ,@(get-going-attrs "no"))) "No :(")
	  (br)
	  (label (input (@ ,@(get-going-attrs "null"))) "Not Sure Yet..."))
	 (fieldset
	  (legend "Meal Choice")
	  (label (input (@ ,@(get-meal-attrs "chicken"))) "Chicken")
	  (br)
	  (label (input (@ ,@(get-meal-attrs "beef"))) "Beef")
	  (br)
	  (label (input (@ ,@(get-meal-attrs "vegetarian"))) "Chicken")))))

(define (route-get-rsvp c)
  ;; TODO: consider a POST instead of GET to prevent people from sharing
  ;; their edit links?
  (let* ((q (uri-query (request-uri (current-request))))
         (name (alist-ref 'rsvp-name q)))
    (send-sxml
      (template-page
       (let ((guests (get-party-by-name name)))
	 (if (not (null? guests))
          `((h2 "RSVP")
            (p "Great news! You're invited :) We can't wait to celebrate with you!")
            (p "We've found the following guests under your name. For each, please "
               "select whether you'll make it and your choice of meal.")
	    (form (@ (action "/rsvp") (method "POST"))
		  ,@(map guest-to-form guests)
		  (button "Save")))
          `((h2 "RSVP")
            (p "Sorry! We can't find anyone under the name '" ,name
               "'. Please double check the spelling and if it looks like a "
               "mistake on our end email us at "
               (a (@ (href "mailto:rsvp@jennex.org")) "rsvp@jennex.org") "."))))))))

(define (route-post-rsvp c)
  (let ((fdata (read-urlencoded-request-data (current-request))))
    (send-sxml
     (template-page
      `((h2 "RSVP")
	(p "Success! Thanks for RSVP-ing.")
	(p (a (@ (href "/rsvp")) "Edit your response"))
	(p (a (@ (href "https://jennex.org")) "Read more about the event")))))))
