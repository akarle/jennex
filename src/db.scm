(import sqlite3
	sql-null
	(chicken condition)
        (chicken string)
	(chicken format))

(define conn (open-database "rsvp.sqlite3"))

(define-record guest id name party-id going meal-choice)
(set-record-printer! guest
  (lambda (x o)
    (fprintf o "(guest id: ~A name: ~A going: ~A meal: ~A)"
             (guest-id x)
             (guest-name x)
             (guest-going x)
             (guest-meal-choice x))))

(define-record party id notes guests)
(set-record-printer! party
  (lambda (x o)
    (fprintf o "(party id: ~A guests: ~A)"
             (party-id x)
             (map (lambda (x) (guest-name x)) (party-guests x)))))

(define (get-guest-by-name name)
  (apply make-guest
	 (first-row conn
"SELECT
  id,
  name,
  party_id,
  going,
  meal_choice
FROM guests WHERE name = $name" name)))

(define (get-guest-by-id id)
  (apply make-guest
	 (first-row conn
"SELECT
  id,
  name,
  party_id,
  going,
  meal_choice
FROM guests WHERE id = $name" id)))

(define (get-guests-in-party pid)
  (map-row make-guest conn
"SELECT
  id,
  name,
  party_id,
  going,
  meal_choice
FROM guests WHERE party_id = $pid" pid))

(define (get-party-by-id pid)
  (let ((res (first-row conn "SELECT id, notes FROM parties WHERE id = $pid" pid)))
    (make-party (car res) (cadr res) '())))

(define (get-party-by-name name)
  ;; Throws <sqlite3 exn> if any queries return empty fails
  (let* ((guest (get-guest-by-name name))
         (party-id (guest-party-id guest))
         (guests (get-guests-in-party party-id))
         (party (get-party-by-id party-id)))
    (party-guests-set! party guests)
    party))

(define (update-guest g)
  (update conn "
          UPDATE guests SET
          name = $1,
          going = $2,
          meal_choice = $3
          WHERE id = $4"
          (guest-name g)
          (guest-going g)
          (guest-meal-choice g)
          (guest-id g)))

(define (update-party-notes pid notes)
  (update conn "UPDATE parties SET notes = $1 WHERE id = $2" notes pid))
