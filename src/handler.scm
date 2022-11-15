;; handler.scm -- define the routes for Spiffy
(import
  spiffy
  uri-common
  intarweb
  sxml-serializer
  sql-null
  (chicken string)
  (chicken condition))

(load "db.scm")

(define email '(a (@ (href "mailto:rsvp@jennex.org")) "rsvp@jennex.org"))

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
         (a (@ (href "/")) "RSVP")
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
  (define (input-name key)
    `(name ,(conc (number->string (guest-id g)) "__" key)))
  (define (get-going-attrs val)
    `(,(input-name "going")
       (value ,val)
       (type "radio")
       (required "true")
       ,@(if (equal? (guest-going g) val) '((checked)) '())))
  (define (get-meal-attrs val)
    `((value ,val)
      ,@(if (equal? (guest-meal-choice g) val) '((selected)) '())))
  ;; TODO: add notes section for allergies, etc
  `((div (@ (class "guest"))
         (fieldset
           (legend ,(guest-name g))
           (label (strong "Name: ")
                  (input (@ ,(input-name "name") (required "true") (value ,(guest-name g)))))
           (p (strong "Will You be Attending?"))
           (label (input (@ ,@(get-going-attrs 1))) "Yes!")
           (br)
           (label (input (@ ,@(get-going-attrs 0))) "No :(")
           (br)
           (label (@ (class "meal-choice"))
                  (strong "Meal Choice:")
                  (select (@ ,(input-name "meal-choice"))
                          (option (@ ,@(get-meal-attrs "")) "-- Please Select if Attending --")
                          (option (@ ,@(get-meal-attrs "chicken")) "Chicken")
                          (option (@ ,@(get-meal-attrs "beef")) "Beef")
                          (option (@ ,@(get-meal-attrs "vegetarian")) "Vegetarian")
                          (option (@ ,@(get-meal-attrs "vegan")) "Vegan")))))))

(define (route-get-rsvp c)
  (call/cc
    (lambda (c)
      (with-exception-handler
        (lambda (exn)
          (print-error-message exn)
          ;; TODO: only print stack trace if NOT a sqlite3 no-data exeption!
          (print-call-chain)
          (send-sxml
            (template-page
              `((h2 "RSVP")
                (p "Sorry! We can't find anyone under that name. "
                   "Please double check the spelling and if it looks like a "
                   "mistake on our end email us at " ,email)
                (p (a (@ (href "/")) "Back")))))
          (c #f))
        (lambda ()
          ;; TODO: consider a POST instead of GET to prevent people from sharing
          ;; their edit links?
          (let* ((q (uri-query (request-uri (current-request))))
                 (name (alist-ref 'rsvp-name q)))
            (send-sxml
              (template-page
                (let ((party (get-party-by-name name)))
                  `((h2 "RSVP")
                    (p "Great news! You're invited :) We can't wait to celebrate with you!")
                    (p "We've found the following guests under your name. For each, please "
                       "select whether you'll make it and your choice of meal.")
                    (form (@ (action "/rsvp") (method "POST"))
                          ,@(map guest-to-form (party-guests party))
                          (h3 "Additional Information")
                          (label (@ (for "notes"))
                                 "Anything else we should know? (Allergies, kids, ...)")
                          (textarea (@ (class "party-notes")
                                       (name ,(conc (party-id party) "__notes"))
                                       (rows 5))
                                    ,(let ((notes (party-notes party)))
                                       (if (sql-null? notes)
                                         ""
                                         notes)))
                          (button (@ (class "party-update")) "Save"))))))))))))

(define key-to-setter
  ;; Just an a-list of input names -> guest-setter functions
  ;; (allowing the request to specify any string is dangerous)
  `(("going" . ,guest-going-set!)
    ("meal-choice" . ,guest-meal-choice-set!)
    ("name" . ,guest-name-set!)))

(define (route-post-rsvp c)
  (call/cc
   (lambda (c)
     (with-exception-handler
      (lambda (exn)
        (print-error-message exn)
        (print-call-chain)
	(send-sxml
	 (template-page
	  `((h2 "RSVP")
	    ;; TODO: log it better...
	    (p "There was an error saving your response please try again "
	       "and if it continues to fail, reach out to us at " ,email))))
	(c #f))
      (lambda ()
	;; The data is in pairs of (ID__key . val), so the first thing to do
	;; is to walk through that data and build up a view of each guest
	(let loop ((guests '())
                   (fdata (read-urlencoded-request-data (current-request)))
                   (notes ""))
	  (if (null? fdata) ;; Done parsing all our form data
	      (begin
                ;; This will raise if update-guest fails (so we get an error page)
                (map update-guest (map cdr guests))
                (let* ((first-guest (cdadr guests))
                       (first-name (guest-name first-guest))
                       (edit-link (conc "/rsvp?rsvp-name=" (uri-encode-string first-name))))
                  (update-party-notes (guest-party-id first-guest) notes)
                  (send-sxml
                    (template-page
                      `((h2 "RSVP")
                        (p "Success! Thanks for RSVP-ing.")
                        (p (a (@ (href ,edit-link))) "Edit your response")
                        (p (a (@ (href "https://jennex.org")) "Read more about the event")))))))
	      (let* ((input (car fdata))
		     (name (car input))
		     (value (cdr input))
		     (split (string-split (symbol->string name) "__"))
		     (id (string->number (car split)))
		     (cached-guest (alist-ref id guests))
		     (key (cadr split))
		     (setter! (alist-ref key key-to-setter equal?)))
                ;; special case notes since it updates the party
                (if (equal? key "notes")
                  (loop guests (cdr fdata) value)
                  (if cached-guest
                    (begin
                      (setter! cached-guest value)
                      (loop guests (cdr fdata) notes))
                    (let ((guest (get-guest-by-id id)))
                      (setter! guest value)
                      (loop (cons (cons id guest) guests) (cdr fdata) notes))))))))))))
