
(setq auto-save-interval 800)
800

auto-save-interval
800

(setq a 100
      b 200
      c 300)

300

a
100
b
200
c
300




(defun count-words-buffer ()
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(forward-word 1)
	(setq count (1+ count)))
      (message "buffer contains %d words." count))))
	




(defun count-words-region (start end)
  (interactive "r")
  (let ((count 0))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(forward-word 1)
	(setq count (1+ count)))
      (message "region contains %d words." count))))

count-words-region



(count-words-buffer)
"buffer contains 58 words."





(/ 7.0 4)
1.75

(/ 7 4)
1


(defun goto-percent (pct)
  (interactive "nGoto percent: ")
  (let* ((size (point-max))
         (charpos (/ (* size pct) 100)))
    (goto-char charpos)))
goto-percent

(defun goto-percent (pct)
  (interactive "nPercent: ")
  (goto-char (/ (* pct (point-max)) 100)))
goto-percent



(defun pluralize (word count)
  (if (= count 1)
      word
    (concat word "s")))
pluralize

(pluralize "goat" 5)
"goats"

(pluralize "change" 1)
"change"

(pluralize "mouse" 6)
"mouses"



(defun pluralize (word count &optional plural)
  (if (= count 1)
      word
    (if (null plural)
        (concat word "s")
      plural)))
pluralize

(pluralize "mouse" 5)
"mouses"

(pluralize "mouse" 5 "mice")
"mice"

(pluralize "mouse" 1 "mice")
"mouse"


(defun how-many (count)
  (cond ((zerop count) "no")
        ((= count 1) "one")
        ((= count 2) "two")
        (t "many")))
how-many

(how-many 1)
"one"
(how-many 2)
"two"
(how-many 3)
"many"


(defun report-change-count (count)
  (message "Made %s %s." (how-many count) (pluralize "change" count)))

report-change-count


(report-change-count 0)
"Made no changes."
(report-change-count 1)
"Made one change."
(report-change-count 2)
"Made two changes."
(report-change-count 3)
"Made many changes."




;; character position of point
(point)
1697

;; character position of mark
(mark)
1761

1


(point-min)
1

(point-max)
1806

;; Whether point is at the beginning of the line (t or nil ).
(bolp)
t


(eolp)
t
;; Whether point is at the end of the line.



(bobp)
nil
;; Whether point is at the beginning of the buffer.




(eobp)
t
;; Whether point is at the end of the buffer.



(insert "hello world")
hello worldnil

hellonil

;; Insert any number of arguments (strings or characters) into the buffer after point.



(string-to-number "1")
1


(char-to-string ?a)
"a"


(substring "hello world" 2 5)
"llo"


(aref "appropriate" 3)
114
;; Array indexing function that can be used to return individual characters from
;; strings; takes an integer argument and returns the character as an integer,
;; using the ASCII code (on most machines).



(interactive "r")
(2404 2548)



(defun remove-outline-marks ()
  "Remove section header marks created in outline-mode."
  (interactive)
  (replace-regexp "^\\*+\\ *" ""))
remove-outline-marks



(defun find-library-file (library)
  "Takes a single argument LIBRARY, being a library file to search for.
Searches for LIBRARY directly (in case relative to current directory,
or absolute) and then searches directories in load-path in order.
It
will test LIBRARY with no added extension, then with .el, and finally
with .elc.
If a file is found in the search, it is visited.
is found, an error is signaled.
If none
Note that order of extension searching
is reversed from that of the load function."(interactive "sFind library file: ")
  (let ((path (cons "" load-path)) exact match elc test found)
    (while (and (not match) path)
      (setq test (concat (car path) "/" library)
            match (if (condition-case nil
                          (file-readable-p test)
                        (error nil))
                      test)
            path (cdr path)))
    (setq path (cons "" load-path))
    (or match
        (while (and (not elc) path)
          (setq test (concat (car path) "/" library ".elc")
                elc (if (condition-case nil
                            (file-readable-p test)
                          (error nil))
                        test)
                path (cdr path))))
    (setq path (cons "" load-path))
    (while (and (not match) path)
      (setq test (concat (car path) "/" library ".el")
            match (if (condition-case nil
                          (file-readable-p test)
                        (error nil))
                      test)
            path (cdr path)))
    (setq found (or match elc))
    (if found(progn
               (find-file found)
               (and match elc
                    (message "(library file %s exists)" elc)
                    (sit-for 1))
               (message "Found library file %s" found))
      (error "Library file \"%s\" not found." library))))


 


(point)
4857  
Most programming probably consists of writing little glue programs,
and for. little glue programs? you can use any language that you’re
already familiar with and that has good libraries for whatever you
need to do.
(point)
5085


(let ((start 4858)
      (end 5085))
  (message "start: %d, end: %d" start end)
  (goto-char start)
  (while (< (point) end)
    (search-forward-regexp "\n")
    (backward-char 1)
    (insert " ")
    (forward-char 1)
    (delete-char 1)))



"start: 4858, end: 5085"


;; (defun my-format ()
;;   (let ((start 4858)
;;         (end 5085))
;;     (goto-char start)
;;     (while (< (point) end)
;;       (search-forward-regexp "\n")
;;       (goto-char (point))
;;       (backward-char 1)
;;       (insert " ")
;;       (backward-delete-char 1))))


my-format
