;;;Write a version of generate that explicitly differentiates between
;;;terminal symbols (those with no rewrite rules) and nonterminal symbols.

(load "code.lisp")

(generate '(sentence a verb))

(defun terminalp (symbol)
  (rewrite phrese))

(defun generate (phrese)
  (cond ((listp phrase)
         (mappend #'generate phrase))
        (terminalp phrase)
         (generate (random-elt choice)))
        (t (list phrase)))
