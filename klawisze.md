Content-Type: text/enriched
Text-Width: 80

<x-color><param>#4f97d7</param>Podstawowe</x-color>

=
- **C-v** screen w dół
- **M-v** screen w górę
- **C-l** tekst na srodku ekranu

- **C-p C-n C-b C-f** jedna literka
- **M-b M-f** słowo

- **C-a** przód linii
- **C-e** koniec linii


- **M-a** przód zdania
- **M-e** koniec zdania

- **M->** koniec pliku
- **M-<<** początek pliku

- **C-u 8 C-p** daje 8 literek do prrodu
- **C-g** canceluje wszystko

- *(load "C:\\Users\\Bobi\\Desktop\\lispu\\land-of-lisp\\graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless  (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                     collect (edge-pair (random-node)
                                        (random-node)))))


(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  (let (visited)
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 (mapc (lambda (edge)
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let (islands)
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (print connected)
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-inslands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list))
          edge-list))



(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                   collect i))
         (edge-list (connect-all-inslands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))

;;(add-cops '((1 (2)) (2 (1) (3)) (3 (2))) '((2 . 3)))

(defun neighbours (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbours a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbours a edge-alist))))



(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                       collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
                       (cond ((eql n wumpus) '(wumpus))
                             ((within-two n wumpus edge-alist) '(blood!)))
                       (cond ((member n glow-worms)
                              '(glow-worm))
                             ((some (lambda (worm)
                                      (within-one n worm edge-alist))
                                    glow-worms)
                              '(lights!)))
                       (when (some #'cdr
                                   (cdr (assoc n edge-alist)))
                         '(sirens!))))))


(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  ;;(setf *player-pos* (find-empty-node))
  ;;(setf *visited-nodes* (list *player-pos*))
  (draw-city))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-edges*))
        (find-empty-node)
        x)))

(defun draw-city ()
  (ugraph->png "C:\\Users\\Bobi\\Desktop\\lispu\\land-of-lisp\\city"
               *congestion-city-nodes*
               *congestion-city-edges*))
*C-x 1** zostawia jedno okienk

- **<<DEL>** usun literke przed ku]rs   Made with   by the communityoerm
- **C-d** usuń literke po kursorze

- **C-w** też WYCINA/ZAIBJA do konca zdania.
        Zaznaczony tekst czy cos

- **M-<<DEL>** Usuns slowo przed kursorem

- **M-D** usuń słowo po kursorze

- **C-k** usuń wszystko do końca linii

- **M-k** usuń wszystko do końca zdania


- **C-<<SPC>** uruchamia zaznaczanie tekstu

- **C-/** undo

- **C-y** WKLEJA
- **M-w** kopiuje
- **C-x h** zaznacza caly buffer


- **M-y** historia zabić. TAm ze możesz zmienić

- **C-x C-s** zapisuje plik
- **C-x k bufname**  Zabija bufera

<x-color><param>#4f97d7</param>SUPERIOR LISP</x-color>

=
- **C-x C-e** wysyla kod

- **C-x b** switch-to-buffer

- **C-c C-z** - zmienia bufer na ten do lispa

- **C-c C-c** - wykonuje wyrażenie przed kursorem
- **C-M-x** - wykonuje całą funkcje
- **C-c C-r** wykonuje zaznaczony region
- **C-c C-k** kompluje i wczytuje cały plik

- **M-.** idzie do definicji symbolu
- **M-,** jak użyłęś M-. to to wraca

- **M-x** slime-pwd wyświetla pwd procesu lispa

- **M-p** poprzednia komenda w replrze


- **C-c M-i** czyści repla

- **C-c C-k** eval whole file

<x-color><param>#2d9574</param>okienka</x-color>

-
- **C-x 1** Jedno okienko
- **C-x 2** okienka horyzontalnie
- **C-x 2** okienka Pionowo
- **C-x o** zmiana aktualnego bufera
- **C-x b** zmiana bufera w okienku
- **C-x ^** okno staje sie wyzsze
- **C-x }** oko staje sie szersze
- **C-x {** oko staje sie chudsze

<x-color><param>#2d9574</param>Hakermen z okienkami</x-color>

-
- **C-x 1**
- **C-x 3**
- **C-x o**


- **C-x b <<RET>**
- **C-x o**

<x-color><param>#2d9574</param>Zaznaczanie</x-color>

-
- **C-M-u** na poczatek s-expression
- **C-M-SPC** zaznacz to s-expression

- **C-M-\** stylizuje kod w caly pliku

- **<<TAB>** stylizuje linie
- **C-M-i** podpowiedz
- **M-^** join line

- **M-(** nowe sexpresson
- **M-)** idz za sexpression
- **C-M-k** wytnij sexpression

-  **M-x sp-cheat-sheet** 


