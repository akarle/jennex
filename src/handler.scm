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

(define key-to-getter
  `(("going" . ,guest-going)
    ("meal-choice" . ,guest-meal-choice)))

(define (guest-to-form g)
  (define (get-attrs key val)
    `((name ,(conc (number->string (guest-id g)) "__" key))
      (value ,val)
      (type "radio")
      (required "true")
      ,@(let ((getter (assoc key key-to-getter)))
          (if (and getter (equal? ((cdr getter) g) val))
	      '((checked))
	      '()))))
  ;; TODO: add notes section for allergies, etc
  `((div (@ (class "guest"))
	 (h4 ,(guest-name g))
	 (fieldset
	  (legend "Will You be Attending?")
	  (label (input (@ ,@(get-attrs "going" 1))) "Yes!")
	  (br)
	  (label (input (@ ,@(get-attrs "going" 0))) "No :("))
	 (fieldset
	  (legend "Meal Choice")
	  (label (input (@ ,@(get-attrs "meal-choice" "chicken"))) "Chicken")
	  (br)
	  (label (input (@ ,@(get-attrs "meal-choice" "beef"))) "Beef")
	  (br)
	  (label (input (@ ,@(get-attrs "meal-choice" "vegetarian"))) "Vegetarian")))))

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
               "mistake on our end email us at " ,email))))))))


(define key-to-setter
  ;; Just an a-list of input names -> guest-setter functions
  ;; (allowing the request to specify any string is dangerous)
  `((going . ,guest-going-set!)
    (meal-choice . ,guest-meal-choice-set!)
    (plus1-going . ,guest-plus1-going-set!)
    (plus1-meal-choice . ,guest-plus1-meal-choice-set!)))

(define (route-post-rsvp c)
  (call/cc
   (lambda (c)
     (with-exception-handler
      (lambda (exn)
        (print-error-message exn)
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
	(let loop ((guests '()) (fdata (read-urlencoded-request-data (current-request))))
	  (if (null? fdata) ;; Done parsing all our form data
	      (begin
                ;; This will raise if update-guest fails (so we get an error page)
                (for-each print (map update-guest (map cdr guests)))
		(send-sxml
		 (template-page
		  `((h2 "RSVP")
		    (p "Success! Thanks for RSVP-ing.")
		    (p (a (@ (href "/")) "Edit your response"))
		    (p (a (@ (href "https://jennex.org")) "Read more about the event"))))))
	      (let* ((input (car fdata))
		     (name (car input))
		     (value (cdr input))
		     (split (string-split (symbol->string name) "__"))
		     (id (string->number (car split)))
		     (cached-guest (assoc id guests))
		     (key (string->symbol (cadr split)))
		     (setter! (cdr (assoc key key-to-setter))))
		(if cached-guest
		    (begin
                      (print "setting to " value)
		      (setter! (cdr cached-guest) value)
		      (loop guests (cdr fdata)))
		    (let ((guest (get-guest-by-id id)))
		      (setter! guest value)
		      (loop (cons (cons id guest) guests) (cdr fdata))))))))))))
