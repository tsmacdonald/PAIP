(defun last-name (name)
  "Ex. 1.1"
  (if (notany (lambda (x) (equal (first (last name)) x))
	    '(md jr sr i ii iii iv v vi vii viii ix x))
      (first (last name))
      (last-name (subseq name 0 (1- (length name))))))

;; Note: Many of these could be tail-recursive. But...they're not. Tough.

(defun power (base pow)
  "Ex. 1.2"
  (cond ((zerop pow) 1)
	((evenp pow) (* (power base (/ pow 2)) (power base (/ pow 2))))
	(t (* base (power base (1- pow))))))

(defun count-atoms (list)
  "Exercise 1.3"
  (if (listp list)
      (if (notany #'listp list)
	  (length list)
	  (+ (count-atoms (first list))
	     (count-atoms (rest list))))
      1))

(defun count-anywhere (item list)
  "Exercise 1.4"
  (if (listp list)
      (loop for i in list summing (count-anywhere item i))
      (if (equal item list)
	  1
	  0)))

(defun dot-product (x y)
  "Exercise 1.5"
  (loop for a in x
        for b in y
        summing (* a b)))
      