(import sqlite3
	sql-null
	(chicken condition)
	(chicken format))

(define conn (open-database "rsvp.sqlite3"))

(define-record guest
  id name party-id going meal-choice
  has-plus1 plus1-going plus1-meal-choice)

(define (get-guest-by-name name)
  (apply make-guest
	 (first-row conn
"SELECT
  id,
  name,
  party_id,
  going,
  meal_choice,
  has_plus1,
  plus1_going,
  plus1_meal_choice
FROM guests WHERE name = $name" name)))

(define (get-guests-in-party pid)
  (map-row make-guest conn
"SELECT
  id,
  name,
  party_id,
  going,
  meal_choice,
  has_plus1,
  plus1_going,
  plus1_meal_choice
FROM guests WHERE party_id = $pid" pid))

;(map guest-name (get-party-by-name "Alex"))
;(map guest-name (get-party-by-name "Sarah"))
;(map guest-name (get-party-by-name "Foo"))
(define (get-party-by-name name)
  ;; get-guest-by-name can throw if none found
  (call/cc
   (lambda (c)
     (with-exception-handler
      (lambda (x) (c '()))
      (lambda ()
	(let* ((guest (get-guest-by-name name))
	       (party-id (guest-party-id guest)))
	  (if (not (sql-null? party-id))
	      (get-guests-in-party party-id)
	      (list guest))))))))

(define (update-guest g)
  (call/cc
   (lambda (c)
     (with-exception-handler
      (lambda (x) (print x) (c #f))
      (lambda ()
	(update conn "
UPDATE guests SET
 name = $1,
 going = $2,
 meal_choice = $3,
 plus1_going = $4,
 plus1_meal_choice = $5
WHERE id = $6"
	  (guest-name g)
	  (guest-going g)
	  (guest-meal-choice g)
	  (guest-plus1-going g)
	  (guest-plus1-meal-choice g)
	  (guest-id g)))))))
