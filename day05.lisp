(defun is-vowel (c)
  (or (char= '#\a c) (char= '#\e c) (char= '#\i c) (char= '#\o c) (char= '#\u c)))

(defun has-vowels (str)
  "Checks if string has at least three different vowels"
  (>= (count-if #'is-vowel str) 3))

(defun appears-twice (str)
  "Checks if string has a character that applies twice in a row"
  (dotimes (idx (1- (length str)))
    (if (char= (aref str idx) (aref str (1+ idx))) (return-from appears-twice t)))
  nil)


(defun appears-twice-sep (str)
  "Checks if string has a character that applies twice with a separating one inside"
  (dotimes (idx (- (length str) 2))
    (if (char= (aref str idx) (aref str (+ idx 2))) (return-from appears-twice-sep t)))
  nil)

(defun contains-bad (str)
  "Checks if the string contains either 'ab', 'cd', 'pq', or 'xy'"
  (dotimes (idx (1- (length str)))
    (let ((nextc (aref str (1+ idx))))
      (case (aref str idx) (#\a (if (char= '#\b nextc) (return-from contains-bad t)))
            (#\c (if (char= '#\d nextc) (return-from contains-bad t)))
            (#\p (if (char= '#\q nextc) (return-from contains-bad t)))
            (#\x (if (char= '#\y nextc) (return-from contains-bad t))))))
  nil)

(defun has-pair-twice (str)
  (dotimes (idx (- (length str) 3))
    (let ((subs (list (aref str idx) (aref str (1+ idx)))))
      (do ((j (+ 2 idx) (1+ j)))
           ((= (1- (length str)) j) j)
          (if (equal subs (list (aref str j) (aref str (1+ j)))) (return-from has-pair-twice t)))))
  nil)

(defun is-nice1 (str)
  "Checks if the string is nice"
  (and (has-vowels str) (appears-twice str) (not (contains-bad str))))

(defun is-nice2 (str)
  (and (appears-twice-sep str) (has-pair-twice str)))

(defun part1 (lst)
  (count-if #'is-nice1 lst))

(defun part2 (lst)
  (count-if #'is-nice2 lst))

(defun read-to-list (n in)
  (if (> n 0) (cons (read-line in) (read-to-list (decf n) in)) ()))


(defun main ()
  (let ((in (open "./5.txt")))
    (let ((lst (read-to-list 1000 in)))
      (print (part1 lst))
      (print (part2 lst))
    (close in))))
