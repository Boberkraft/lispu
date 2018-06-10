(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
	(dolist (cd *db*)
		;; ~10t ; daj tyle spacji by dac do 10 kolumny
		;; ~a ladny print. a e s t h e t i c. Pochlania jeden argument!
		;; ~{xxx~} powtarzaj to x az sie skonczy
		;; ~% nowa linia
		(format t "~{~a:~10t~a~%~}~%" cd)))

(defun make-cd (title artist rating ripped)
	(list :tile title :artist artist :rating rating :ripped ripped))

(defun prompt-read (prompt)
	(format *query-io* "~a: " prompt)
	(force-output *query-io*)
	(read-line *query-io*))

(defun save-db (filename)
	(with-open-file (out filename
					 :direction :output
					 :if-exists :supersede)
		(with-standard-io-syntax 
			(print *db* out))))

(defun load-db (filename)
	(with-open-file (in filename)
		(with-standard-io-syntax
			(setf *db* (read in)))))

(defun prompt-for-cd () 
	(make-cd
		(prompt-read "Title")
		(prompt-read "Artist")
		(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
		(y-or-n-p "Ripped")))

(defun add-cds ()
	(loop (add-record (prompt-for-cd))
		(if (not (y-or-n-p "Another?")) (return))))

(defun select-by-artist (artist)
	(remove-if-not 
		#'(lambda (cd) (equal (getf cd :Artist) "Tyszko")) 
		*db*))

(defun select (selector-fn)
	(remove-if-not selector-fn *db*))

;; Se moge tak wywolac
;; (select #' (lambda (cd) (equal (getf cd :artist) "Tyszko")))

;; ale tutaj se stworzylem fun ktora zwraca mi lambde szukajaca tego artyste
;; jako iz
(defun artist-selector (artist)
	#'(lambda (cd) (equal (getf cd :artist) artist)))

;; tutaj uzycie
;; no i wpierw wykonuja sie parametry, a to lambde zwraca!
(select (artist-selector "Tyszko"))

;; ale my jestesmy koksy robimy tak
;; jezeli title jest nil to zwroci true
;; a jak nie to zrobi geta

;;(defun where (&key title artist rating (ripped nil ripped-p))
;;	#'(lambda (cd)
;;		(and
;;			(if title	 (equal (getf cd :title)  title)	t)
;;			(if artist	 (equal (getf cd :artist) artist) 	t)
;;			(if rating	 (equal (getf cd :rating) rating)  	t)
;;			(if ripped-p (equal (getf cd :ripped) ripped)	t))))



(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
	(setf *db* ;; zapisz do bazy
		(mapcar  ;; na kazdym wpisie
			#'(lambda (row) ;; zrob lambde
				(when (funcall selector-fn row) ;; teraz na kazdym wpierszy
					;; jezeli uzupelniles tytul, to robi geta, i potem zamienia wartosc
					;; ...
					;; to wtedy aktualizuje ten wiersz
					;; i wtedy zaaktualizowany wiersz idzie do mapcara
					;; i on robi nowy *db* kurcze
					(if title	 (setf (getf row :title)  title))
					(if artist   (setf (getf row :artist) artist))
					(if rating    (setf (getf row :rating) rating))
					(if ripped-p (setf (getf row :ripped) ripped)))
				row) *db*)))

(defun delete-rows (selector-fn)
	(setf *db* (remove-if selector-fn *db*)))

(add-cds)
(update (where :artist "Tyszko") :rating 11)
(select (where :artist "Tyszko"))
(save-db "D:/Piosenki.txt")
(load-db "D:/Piosenki.txt")
*db*


(defmacro backwards (expr) (reverse expr))

(backwards ("hello, world" t format))

(defun make-comparison-expr (field value)
	`(equal (getf cd ,field) ,value))

(defun make-comparison-lists (fields)
	(loop while fields
		collecting (make-comparison-expr (pop fields) (pop fields))))

(make-compresion-expr :rating 10)

(defmacro where (&rest clauses)
	`#'(lambda (cd) (and ,@(make-comparison-lists clauses))))
(where :artist "Tyszko")
(macroexpand-1 '(where :artist "Tyszko"))
(select (where :artist "Tyszko"))