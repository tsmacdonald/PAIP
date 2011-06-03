(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *bigger-grammar*
    '((sentence -> (noun-phrase verb-phrase))
      (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
      (verb-phrase -> (Verb noun-phrase PP*))
      (PP* -> () (PP PP*))
      (Adj* -> () (Adj Adj*))
      (PP -> (Prep noun-phrase))
      (Prep -> to in by with on above below through)
      (Adj -> big little blue green adiabatic spunky vernacular)
      (Article -> the a)
      (Name -> Pat Kim Lee Terry Robin Vesuvius Le-a)
      (Noun -> man ball woman table unicorn spaceship)
      (Verb -> hit took saw liked)
      (Pronoun -> he she it these those that)))

(defvar *grammar* *simple-grammar*)

(setf *grammar* *bigger-grammar*)

(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

(defun random-elt (seq)
  "Returns a random element from the given sequence."
  (elt seq (random (length seq))))

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase) ;; Version from the book
  "Generate a random sentence or phrase."
  (cond ((listp phrase)
	 (format t "~&Case 1 - ~a" phrase)
	 (mappend #'generate phrase))
	((rewrites phrase)
	 (format t "~&Case 2 - ~a" phrase)
	 (generate (random-elt (rewrites phrase))))
	(t (format t "~&Case 3 - ~a" phrase) (list phrase))))

(defun generate2 (phrase)
  "Generate a random sentence or phrase. (Exercise 2.1)"
  ;; This version kinda cheats....
  (cond ((listp phrase)
	 (mappend #'generate2 phrase))
	((assoc phrase *grammar*)
	 (generate2 (random-elt (rewrites phrase))))
	(t (list phrase))))

(defun generate3 (phrase)
  "Generate a random sentence or phrase. (Exercise 2.1)"
  ;; Norvig's non-cheating version (written from memory
  ;; several days after I read it)
  (let ((rewrites nil))
    (cond ((listp phrase)
	   (mappend #'generate3 phrase))
	  ((setq rewrites (rewrites phrase))
	   ;This is the first time I've appreciated Lisp-2s
	   (generate3 (random-elt rewrites)))
	  (t (list phrase)))))

(defun generate4 (phrase)
  "Gen[tab]. (Exercise 2.2)"
  (cond ((listp phrase)
	 (mappend #'generate4 phrase))
	((non-terminal-p phrase)
	 (generate4 (random-elt (rewrites phrase))))
	(t (list phrase))))

(defun non-terminal-p (category)
  "Returns true if the given category is a real category.
   Nil, therefore, indicates that it's just a word."
  (not (null (rewrites category))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
   with a complete parse tree."
  (cond ((listp phrase)
	 (mapcar #'generate-tree phrase))
	((rewrites phrase)
	 (cons phrase
	       (generate-tree (random-elt (rewrites phrase)))))
	(t (list phrase))))

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
	((listp phrase)
	 (combine-all (generate-all (first phrase))
		      (generate-all (rest phrase))))
	((rewrites phrase)
	 (mappend #'generate-all (rewrites phrase)))
	(t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
   E.g., (combine-all '((a) (b)) '((1) (2))) -> ((A 1) (B 1) (A 2) (B 2))"
  (mappend #'(lambda (y)
	       (mapcar #'(lambda (x) (append x y)) xlist))
	   ylist))

(defun cross-product (op x y) ;; Unfortunately not as stylish as the mappend/mapcar approach
  "Exercise 2.4"
  (let ((result ()))
    (dolist (b y)
      (dolist (a x)
	(push (funcall op a b) result)))
    (nreverse result)))

(defun combine-all2 (x y)
  "Exercise 2.4"
  (cross-product #'list x y))

(defparameter *lisp-grammar* ;;Massively hacky and simplified grammar for a generic Lisp. Ex 2.3.
  '((expression -> (function-call) (self-evaluating-exp))
    (expr* -> () (expression expr*))
    (function-call -> (opener function-with-args closer))
    (opener -> \()
    (closer -> \))
    (function-with-args -> (let+args) (lambda+args) (cons+args) (car+args) (cdr+args)
                           (cond+args) (map+args) (apply+args) (reduce+args) (eval+args) (quote+args))
                 ;; or special form or macro or whatever - function is a loose term here
    (arg -> (expression))
    (args* -> () (arg args*))
    (let+args -> ((let opener assign* closer)))
    (lambda+args -> ((lambda opener sym* closer expr*)))
    (cons+args -> (cons expression expression))
    (car+args -> (car expression))
    (cdr+args -> (cdr expression))
    (cond+args -> (cond opener cond* closer))
    (condition -> (expression expression))
    (cond* -> () (condition cond*))
    (map+args -> (map symbol list))
    (apply+args -> (apply symbol list))
    (reduce+args -> (reduce symbol list))
    (eval+args -> (eval expression))
    (quote+args -> (quote expression))
    (list -> (opener sym* closer) (function-call))
    (symbol -> ((char*)))
    (sym* -> () (symbol whitespace sym*))
    (self-evaluating-exp -> (number) (string) (char))
    (number -> (digit digit*))
    (digit -> (1) (2) (3) (4) (5) (6) (7) (8) (9) (0))
    (digit* -> () (digit digit*))
    (string -> (string-opener text string-closer))
    (string-opener -> (\"))
    (string-closer -> (\"))
    (text -> (char*) (char*) (whitespace)) ;;Stacking the odds a bit
    (char -> (a) (b) (c) (d) (e) (f) (g) (h) (i) (j) (k) (l) (m)
             (n) (o) (p) (q) (r) (s) (t) (u) (v) (w) (x) (y) (z)
             (\.) (\,) (\')  (!) (+) (-) (_) (?))
    (char* -> () (char char*))
    (whitespace -> (| |))))
