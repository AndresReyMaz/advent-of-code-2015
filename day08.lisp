(defun count-escaped (str)
  "Counts the number of backslash-escaped characters"
  (let ((ans 0))
    (dotimes (idx (1- (length str)))
      (let ((cur (char str idx))
            (nxt (char str (1+ idx))))
        (if (and (char= #\\ cur) (or (not (char= #\x nxt)) (not (is-hex (char str (+ 2 idx))))))
            (progn (incf idx)
                   (incf ans)))))
    ans))

(defun is-hex (c)
  "Checks if character is hex"
  (or (and (char>= c #\a) (char<= c #\f)) (and (char>= c #\0) (char<= c #\9))))

(defun count-hex (str)
  "Counts the number of hex characters"
  (let ((ans 0))
    (dotimes (idx (1- (length str)))
      (let ((cur (char str idx))
            (nxt (char str (1+ idx))))
        (if (and (char= #\\ cur) (char= #\x nxt) (is-hex (char str (+ 2 idx))))
            (incf ans))))
    ans))

(defun chars-to-represent (str)
  "Counts the number of characters to represent, ignoring the enclosing quote marks"
  (let ((ans (length str)))
    (dotimes (idx (1- (length str)))
      (let ((cur (char str idx)))
        (if (or (char= #\\ cur) (char= #\" cur))
            (if (not (eql 0 idx)) (incf ans)))))
    ans))

(defun part1 (lst)
  (reduce #'+ (map 'list #'(lambda (str)
                             (- (length str)
                                (- (length str) 2 (count-escaped str) (* (count-hex str) 3))))
                   lst)))

(defun part2 (lst)
  (reduce #'+ (map 'list #'(lambda (str)
                             (- (+ (chars-to-represent str) 4) (length str)))
                   lst)))

(defun read-to-list (n in)
  (if (> n 0) (cons (read-line in) (read-to-list (decf n) in)) ()))


(defun main ()
  (let ((in (open "./8.txt")))
    (let ((lst (read-to-list 300 in)))
      (print (part1 lst))
      (print (part2 lst))
    (close in))))

