(+ 2 2)
'(this is a quoted list)
(+ 2 (+ 2 2))

fill-column
(fill-column)

(concat "abc" "def")

(substring "The quick brown fox jumped." 16 19)

(+ 2 fill-column)

(concat "The " (number-to-string (+ 2 fill-column)) " red foxes.")

(+)
(*)

(+ 3)
(* 3)

(+ 3 4 5)
(* 3 4 5)

(+ 2 'hello)

(number-or-marker-p 3)
(zerop 0)
(listp ())

(message "This message appears in the echo area!")
(message "The name of this buffer is: %s." (buffer-name))

(message "The value of fill-column is %d." fill-column)

(message "He saw %d %s"
	 (- fill-column 32)
	 (concat "red "
		 (substring
		  "The quick brown foxes jumped." 16 21)
		 " leaping."))

a
(a b c)

(setq counter 0)
(setq counter (+ counter 2))

(buffer-name)  "test.el"
(buffer-file-name) "/Users/michael/emacs/lisp/elisp/test.el"
(buffer-size) 1107
(point)					;816
counter

(message "hello world")


(self-insert-command 1)

(buffer-name)
(buffer-file-name) "/Users/michael/emacs/lisp/elisp/test.el"

(+ 2 2) 				;4

(current-buffer)   #<buffer test.el>

(other-buffer) #<buffer hack>

(switch-to-buffer (other-buffer))

(buffer-size)

(point)  977

(point-min)
(point-max)


(defun multiply-by-seven (number)
  "Multiply NUMBER by seven."
  (* number 7))

(multiply-by-seven 3)

(defun *3 (michael)
  "3 michael"
  (* michael 3))			;*3

(*3 3)					;

(defun multiply-by-seven (number)	; Interactive version.
  "Multiply NUMBER by seven."
  (interactive "p")
  (message "The result is %d" (* number 7)))


(message "The result is %d" (* 5 7))

(let ((zebra "stripes")
      (tiger "fierce"))
  (message "One kind of animal has %s and another is %s."
	   zebra tiger))

(let ((birch 3)
      pine
      fir
      (oak 'some))
  (message
   "Here are %d variables with %s, %s, and %s value."
   birch pine fir oak))



(if (> 5 4)
    (message "5 is greater then 4!"))

(defun type-of-animal (characteristic)
  "Print message in echo area depending on CHARACTERISTIC.
If the characteristic is the string \"fierce\",
then warn of a tiger."
  (if (equal characteristic "fierce")
      (message "It is a tiger!")))

(type-of-animal "fierce")
(type-of-animal "striped")

(if (> 4 5)                               ; if-part
    (message "4 falsely greater than 5!") ; then-part
  (message "4 is not greater than 5!"))   ; else-part

(defun type-of-animal (characteristic)  ; Second version.
  "Print message in echo area depending on CHARACTERISTIC.
     If the CHARACTERISTIC is the string \"fierce\",
     then warn of a tiger; else say it is not fierce."
  (if (equal characteristic "fierce")
      (message "It is a tiger!")
    (message "It is not fierce!")))

(type-of-animal "fierce")

(type-of-animal "striped")

(if 4
    'true
  'false)

(if nil
    'true
  'false)

(> 4 5)					;nil
(> 5 4)					;t


(defun double (number)
  "double NUMBER"
  (* number 2))

(double 5)

(defun double (number)
  "double NUMBER"
  (interactive "p")
  (message "the result of double is %d: " (* number 2)))

(defun test-fill-column (number)
  "test the NUMBER is greater than fill-column or not"
  (interactive "p")
  (if (> number fill-column)
      (message "the %d is greater than %d."
	       number fill-column)
    (message "the %d is equal to or lesser than %d."
	     number fill-column)))


(defun simplified-beginning-of-buffer ()
  "Move point to the beginning of the buffer;
leave mark at previous position."
  (interactive)
  (push-mark)
  (goto-char (point-min)))

(push-mark)


(defun append-to-buffer (buffer start end)
  ""
  (interactive
   (list (read-buffer "Append to buffer: " (other-buffer
					    (current-buffer) t))
	 (region-beginning) (region-end)))
  (let ((oldbug (current-buffer)))
    (save-excursion
      (let* ((append-to (get-buffer-create buffer))
	     (windows (get-buffer-window-list append-to t t))
	     point)
	(set-buffer append-to)
	(setq point (point))
	(barf-if-buffer-read-only)
	(insert-buffer-substring oldbuf start end)
	(dolist (window windows)
	  (when (= (window-point window) point)
	    (set-window-point window (point))))))))


(defun buffer-exists-test (buffer)
  "enter a BUFFER name to test if the BUFFER exist"
  (interactive
   (list (read-buffer "enter a buffer name: " (other-buffer
					    (current-buffer) t))))
  (if (get-buffer buffer)
      (message "buffer exists.")
    (message "buffer doesn't exist.")))

(defun insert-buffer-test (buffer)
  ""
  (interactive "*bInsert buffer: ")
  (or (bufferp buffer)
      (setq buffer (get-buffer buffer)))
  (let (start end newmark)
    (save-excursion
      (save-excursion
	(set-buffer buffer)
	(setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark)))


;; (defun beginning-of-buffer (&optional arg)
;;   "documentation.."



(if (> (buffer-size) 10000)
    ;; avoid overflow for large buffer sizes!
    (* (prefix-number-value arg)
       (/ size 10))
  (/ (+ 10
	(* size
	   (prefix-numeric-value arg)))
     10))

(setq aaa 10)

(defun beginning-of-buffer (&optional arg)
  "Move point to the beginning of the buffer;
     leave mark at previous position.
     With \\[universal-argument] prefix,
     do not set mark at previous position.
     With numeric arg N,
     put point N/10 of the way from the beginning.
     
     If the buffer is narrowed,
     this command uses the beginning and size
     of the accessible part of the buffer.
     
     Don't use this command in Lisp programs!
     \(goto-char (point-min)) is faster
     and avoids clobbering the mark."
  (interactive "P")
  (or (consp arg)
      (and transient-mark-mode mark-active)
      (push-mark))
  (let ((size (- (point-max) (point-min))))
    (goto-char (if (and arg (not (consp arg)))
		   (+ (point-min)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (+ 10 (* size (prefix-numeric-value arg)))
			   10)))
		 (point-min))))
  ;; This puts the cursor at the beginning of the first line after the appropriate tenths position in the buffer
  (if (and arg (not (consp arg))) (forward-line 1)))


(forward-line 2000)


(defun optional-test (&optional arg)
  "pass in a NUMBER to compare it with the fill-column"
  (interactive "P")
  (if (and arg (not (consp arg)))
      (if (> (prefix-numeric-value arg) fill-column)
	  (message "arg is greater than fill-column")
	(if (< (prefix-numeric-value arg) fill-column)
	    (message "arg is less than fill-column")
	  (message "arg equals to fill-column")))
    (let ((arg 56))
      (if (> arg fill-column)
	  (message "56 is greater than fill-column")
	(if (< arg fill-column)
	    (message "56 is less than fill-column")
	  (message "56 equals to fill-column"))))))

;; how to check the arg is passed or not?

fill-column				;70

      
(defun what-line ()
  ""
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (message "Line %d"
	       (1+ (count-lines 1 (point)))))))

(count-lines 1 (point))			;313
(point)					;7029
;; exercise
(defun show-the-first-60-characters ()
  "show the first 60 characters even if the buffer is narrowd"
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (message "The first 60 characters: %s"
	       (buffer-substring (point-min) 60)))))


(car '(rose violte daisy buttercup))
rose

(cdr '(rose violte daisy buttercup))
(violte daisy buttercup)


(cons 'pine '(fir oak maple))
(pine fir oak maple)

(length '(buttercup))
1

(length '(daisy buttercup))
2

(length ())
0

(length)
      
(cdr '(pine fir oak maple))		;(fir oak maple)
(cdr '(a))				;nil
(cdr 'nil)				;nil
(cdr ())				;nil

(nthcdr 2 '(pine fir oak maple))	;(oak maple)
(cdr (cdr '(pine fir oak maple)))	;(oak maple)

(nth 2 '(a b c))			;c


(setq animals '(antelope giraffe lion tiger))
animals					;(antelope giraffe lion tiger)

(setcar animals 'hippopotamus)
animals					;(hippopotamus giraffe lion tiger)

(setq domesticated-animals '(horse cow sheep goat))
(setcdr domesticated-animals '(cat dog))
domesticated-animals

;; exercise
(cons 'bird-a '(bird-base))		;(bird-a bird-base)
(setq bird (cons 'bird-c (cons 'bird-b (cons 'bird-a '(bird-base)))))
bird					;(bird-c bird-b bird-a bird-base)
(cons bird bird)			;((bird-c bird-b bird-a bird-base) bird-c bird-b bird-a bird-base)

(setcar bird 'fish)
bird					;(fish bird-b bird-a bird-base)
(setcdr bird '(fish1 fish2 fish3))
bird					;(fish fish1 fish2 fish3)

(search-forward "a" nil t 30)		

(defun zap-to-char (arg char)
  (interactive "p\ncZap to char: ")
  (if (char-table-p translation-table-for-input) ;translation-table-for-input obsolete in 23.1
      (setq char (or (aref translation-table-for-input char) char)))
  (kill-region (point) (progn
			 (search-forward (char-to-string char)
					 nil nil arg)
			 (point))))



(unless t)				;nil
(if t
    (message "true"))			;"true"

(if (not t)
    (message "true"))			;nil



(defun kill-region (beg end)
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (condition-case
      nil
      (let ((string (filter-buffer-substring beg end t)))
	(when string			;STRING is not nil
	  (if (eq last-command 'kill-region)
	      (kill-append string (< end beg) yank-handler)
	    (kill-new string nil yank-handler)))
	(when (or string (eq last-command 'kill-region))
	  (setq this-command 'kill-region))
	nil)
    ((buffer-read-only text-read-only)
     (copy-region-as-kill beg end)
     (setq this-command 'kill-region)
     (if kill-read-only-ok
	 (progn (message "Read only text copied to kill ring") nil)
       (barf-if-buffer-read-only)
       (singal 'text-read-only (list (current-buffer)))))))

 
(defun kill-region (beg end)

  (interactive (list (point) (mark)))	;done

  (unless (and beg end)			;done
    (error "The mark is not set now, so there is no region"))
  
  (condition-case nil			;done
      
      (let ((string (filter-buffer-substring beg end t))) ;done

	;; second argument of condition-case
	(when string					  ;string not nil
	  (if (eq last-command 'kill-region)		  
	      (kill-append string (< end beg) yank-handler) ;old kill-append function, append
	    (kill-new string nil yank-handler)))	    ;old kill-new function, replace

	(when (or string (eq last-command 'kill-region)) ;last-command is kill-region
	  (setq this-command 'kill-region))

	nil)				;???

    ;; third argument of condition-case
    ((buffer-read-only text-read-only)	;the if-part, condition-names of an error handler
     ;; then-part
     (copy-region-as-kill beg end)	;done
     (setq this-command 'kill-region)	;done
     (if kill-read-only-ok		;done
	 (progn (message "Read only text copied to kill ring") nil)
       (barf-if-buffer-read-only)	;done
       (signal 'text-read-only (list (current-buffer)))) ;done
     )
    
    ))


    
this-command

last-command

(buffer-read-only text-read-only)
buffer-read-only

(defun copy-region-as-kill (beg end)
  (interactive "r")			;region
  (if (eq last-command 'kill-region)
      (kill-append (filter-buffer-substring beg end) (< end beg))
    ;; (< end beg)
    ;; This expression does not directly determine whether the killed text in this command is located before or after the kill text of the last command; what it does is determine whether the value of the variable end is less than the value of the variable beg. If it is, it means that the user is most likely heading towards the beginning of the buffer. Also, the result of evaluating the predicate expression, (< end beg), will be true and the text will be prepended before the previous text. On the other hand, if the value of the variable end is greater than the value of the variable beg, the text will be appended after the previous text.
    (kill-new (filter-buffer-substring beg end)))
  (if transient-mark-mode
      (setq deactivate-mark t))
  nil)					;?
   

(concat "abc" "def")			;"abcdef"
(concat "new "
	(car '("first element" "second element"))) ;"new first element"
(concat (car '("first element" "second element"))
	" modified")			;"first element modified"



(nthcdr 1 '(1 2 3 4))			;(2 3 4)
(nthcdr 2 '(1 2 3 4))			;(3 4)

(setq my-kill-ring '(1 2 3 4 5 6 7 8 9 10 11)) ;(1 2 3 4 5 6 7 8 9 10 11)

max=5
(setcdr
 (nthcdr (1- 5) my-kill-ring)		;(5 6 7 8 9 10 11)
 nil)					;nil
my-kill-ring				;(1 2 3 4 5)



(defun kill-new (string &optional replace yank-handler)
  ""
  ;; string not nil
  (if (> (length string) 0)
      (if yank-handler
	  (put-text-property 0 (length string)
			     'yank-handler yank-handler string))
    (if yank-handler
	(singal 'args-out-of-range
		(list string "yank-handler specified for empty string"))))
  (if (fboundp 'menu-bar-update-yank-menu)
      (menu-bar-update-yank-menu string (and replace (car kill-ring))))
  (if (and replace kill-ring)		;
      (setcar kill-ring string)
    (push string kill-ring)		;replace or kill-ring is nil
    (if (> (length kill-ring) kill-ring-max)
	(setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
  (setq kill-ring-yank-pointer kill-ring)
  (if interprogram-cut-function
      (funcall interprogram-cut-function string (not replace))))


(defun test-search (arg)
  (interactive "MString to search: ")
  (if arg
      (progn
	(search-forward arg nil nil)
	(message "Found!"))))



(defun test-3rd ()
  (interactive)
  (if (< (length kill-ring) 3)
      (message "element less than 3")
    (message "the 3rd element in kill-ring: %s"    (nth 2 kill-ring))))


    
(nth 2 kill-ring)
(length kill-ring)


(setq flowers '(violet buttercup))
(cons 'flower-a flowers)
(cons 'flower-b (cons 'flower-a flowers))
(setq more-flowers (cons 'flower-b (cons 'flower-a flowers)))
more-flowers				;(flower-b flower-a violet buttercup)


kill-ring

kill-ring-yank-pointer

(length kill-ring)			;7


(let ((minev '(1 2 3 4 5 6)))
  (message "first: %d. second: %d. thrid: %d. fourth: %d."
	   (car minev)
	   (car (nthcdr 1 minev))
	   (car (nthcdr 2 minev))
	   (car (nthcdr 3 minev))))




(setq empty-list ())
empty-list
(setq animals '(gazelle giraffe lion tiger))
animals


(setq animals '(gazelle giraffe lion tiger))
     
(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(print-elements-of-list animals)


(defun total-of-triangle (number-of-rows)
  "add all the pebbles in each of the number of rows up"
  (let ((total 0)
	(row-number 1))
    (while (<= row-number number-of-rows)
      (setq total (+ total row-number))
      (setq row-number (1+ row-number)))
    total))


(total-of-triangle 4)			;10
(total-of-triangle 7)			;28

(defun triangle (number-of-rows)
  "decrement method to add all the pebbles up"
  (let ((total 0))
    (while (> number-of-rows 0)
      (setq total (+ total number-of-rows))
      (setq number-of-rows (1- number-of-rows)))
    total))


(triangle 4)				;10
(triangle 7)				;28

(setq animal '(gazelle giraffe lion tiger))
animal					;(gazelle giraffe lion tiger)
(reverse animal)			;(tiger lion giraffe gazelle)

(defun reverse-list-with-while (list)
  ""
  (let (value)
    (while list
      (setq value (cons (car list) value))
      (setq list (cdr list)))
    value))

animal					;(gazelle giraffe lion tiger)
(reverse-list-with-while animal)	;(tiger lion giraffe gazelle)

(defun reverse-list-with-dolist (list)
  ""
  (let (value)
    ;; return value
    (dolist (element list value)	;each time take the first element
      (setq value (cons element value)))))

(reverse-list-with-dolist animal)	;(tiger lion giraffe gazelle)

(let (value)
  ;; return value
  (dotimes (number 3 value)		;start from 0
    (setq value (cons number value))))
;; (2 1 0)



(defun triangle-using-dotimes (number-of-rows)
  ""
  (let ((total 0))
    (dotimes (number number-of-rows total) ;return total
      (print number)			   ;tough debug
      (setq total (+ total (1+ number)))))) ;row start from 0!!!

(triangle-using-dotimes 4)		;10
;; 
0

1

2

3
10



(setq animals '(gazelle giraffe lion tiger))

(defun print-elements-recursively (list)
  ""
  (when list				;do-again-test
    (print (car list))			;body
    (print-elements-recursively		;recursive call
     (cdr list))))			;next-step-expression

(print-elements-recursively animals)

gazelle

giraffe

lion

tiger
nil


(defun triangle-recursively (number)
  "Return the sum of the number 1 through NUMBER inclusive.
Uses recursion"
  (if (= number 1)			;do-again-test
      1					;then-part
    (+ number				;else part
       (triangle-recursively		;recursive call
	(1- number)))))			;next-step-expression

(triangle-recursively 7)		;28


(defun triangle-using-cond (number)
  (cond ((<= number 0) 0)
	((= number 1) 1)
	((> number 1)
	 (+ number (triangle-using-cond (1- number))))))

(triangle-using-cond 7)			;28

(defun square-each (numbers-list)
  "Square each of a NUMBER LIST, recursively."
  (if (not numbers-list)
      nil
    (cons
     (* (car numbers-list) (car numbers-list))
     (square-each (cdr numbers-list)))))

(square-each '(1 2 3))			;(1 4 9)

(defun add-elements (numbers-list)
  "Add the elements of NUMBERS-LIST together."
  (if (not numbers-list) 0
    (+ (car numbers-list)
       (add-elements (cdr numbers-list)))))

(add-elements '(1 2 3 4))		;10


(defun keep-three-letter-words (word-list)
  "Keep three letter words in WORD-LIST."
  (cond
   ((not word-list) nil)
   ((eq 3 (length (symbol-name (car word-list))))
    (cons (car word-list) (keep-three-letter-words (cdr word-list))))
   (t (keep-three-letter-words (cdr word-list)))))

(keep-three-letter-words '(one two three four five six))
;; (one two six)


(defun triangle-initialization (number)
  "Return the sum of the numbers 1 through NUMBER inclusive.
This is the initialization component of a two function 
duo that uses recursion."
  (triangle-recursive-helper 0 0 number))

(defun triangle-recursive-helper (sum counter number)
  "Return SUM, using COUNTER, through NUMBER inclusive.
This is the helper component of a two function duo that uses recursion."
  (if (> counter number)
      sum
    (triangle-recursive-helper (+ sum counter) ;sum
			       (1+ counter)    ;counter
			       number)))       ;number


(triangle-initialization 2)		;3
(triangle-initialization 3)		;6
(triangle-initialization 4)		;10



   
(defun total-of-triangle-square (number-of-rows)
  "add all the pebbles in each of the number of rows up"
  (let ((total 0)
	(row-number 1))
    (while (<= row-number number-of-rows)
      (setq total (+ total (* row-number row-number)))
      (setq row-number (1+ row-number)))
    total))

(total-of-triangle-square 4)		;30



(defun total-of-triangle-multiply (number-of-rows)
  "multiply all the pebbles in each of the number of rows up"
  (let ((total 1)
	(row-number 1))
    (while (<= row-number number-of-rows)
      (setq total (* total row-number))
      (setq row-number (1+ row-number)))
    total))

(total-of-triangle-multiply 4)		;24
(total-of-triangle-multiply 2)		;2



(defun triangle-using-cond-square (number)
  (cond ((<= number 0) 0)
	((= number 1) 1)
	((> number 1)
	 (+ (* number number)
	    (triangle-using-cond-square (1- number))))))

(triangle-using-cond-square 4)		;30


(sentence-end)
"\\([.?!…‽][]\"'”’)}]*\\($\\|[  ]$\\|	\\|[  ][  ]\\)\\|[。．？！]+\\)[  	
]*"					


(defun forward-sentence (&optional arg)
  "document"
  (interactive "p")
  (or arg (setq arg 1))
  (let ((opoint (point))
	(sentence-end (sentence-end)))
    (while (< arg 0)
      (let ((pos (point))
	    (par-beg (save-excursion (start-of-paragraph-text) (point))))
	(if (and (re-search-backward sentence-end par-beg t) ;found
		 (or (< (match-end 0) pos)		     ;backward 
		     (re-search-backward sentence-end par-beg t)))
	    (goto-char (match-end 0))	;found
	  (goto-char par-beg)))		;not found
      (setq arg (1+ arg)))
    (while (> arg 0)
      (let ((par-end (save-excursion (end-of-paragraph-text) (point))))
	(if (re-search-forward sentence-end par-end t)
	    (skip-chars-backward " \t\n")
	  (goto-char par-end)))
      (setq arg (1- arg)))
    (constrain-to-field nil opoint t)))


abcd adg edfads
(skip-chars-backward " \t\n")


	  
fill-prefix				;nil
paragraph-ignore-fill-prefix		;t


(let ((foo 2))
  (while (> foo 0)
    (insert (format "foo is %d. \n" foo))
    (setq foo (1- foo))))
foo is 2. 
foo is 1. 

(insert "hello world")

hello world

(defun search-for-black-lines ()
  "search for two or more black lines"
  (interactive)
  (re-search-forward "\n\n\n"))

(search-for-black-lines)

hello
hello

hello


hello



hello


  
(defun name-of-function (argument-list)
  "documentation..."
  (interactive-expression...)
  (body...))

;; (defun count-words-example (beginning end)
;;   "count words between beginning and end"
;;   (interactive "r")
;;   (message "Counting words in region ...")

;;   ;; 1. set up appropriate conditions.
;;   (save-excursion
;;     (goto-char beginning)
;;     (let ((count 0))
;;       ;; 2. run the while loop
;;       (while (< (point) end)
;; 	(re-search-forward "\\w+\\W*")
;; 	(setq count (1+ count)))
;;       ;; 3. send a message to the user
;;       (cond ((zerop count)
;; 	     (message "The region does NOT have any words."))
;; 	    ((= count 1)
;; 	     (message "The region has 1 word."))
;; 	    (t
;; 	     (message "The region has %d words." count))))))


      
		  
    
hello







world



(defun count-words-example (beginning end)
  "count words between beginning and end"
  (interactive "r")
  (message "Counting words in region ...")

  ;; 1. set up appropriate conditions.
  (save-excursion
    (goto-char beginning)
    (let ((count 0))
      ;; 2. run the while loop
      ;; and region restriction and search test in while
      (while (and (< (point) end)	
		  (re-search-forward "\\w+\\W*" end t))
	(setq count (1+ count)))
      ;; 3. send a message to the user
      (cond ((zerop count)
	     (message "The region does NOT have any words."))
	    ((= count 1)
	     (message "The region has 1 word."))
	    (t
	     (message "The region has %d words." count))))))

hello


world


(defun count-words-example (beginning end)
  ""
  (interactive "r")
  ;; 1. set up appropriate conditions
  (message "Counting words in region.")
  (save-excursion
    (goto-char beginning)
    ;; 2. count the words
    (let ((count (recursive-count-words end)))
      ;; 3. send a message to the user
    (cond ((zerop count)
	   (message "The region does NOT have any words."))
	  ((= 1 count)
	   (message "The region has 1 word."))
	  (t
	   (message "The region has %d words." count))))))


(defun recursive-count-words (region-end)
  ""
  (if (and (< (point) region-end)
	   (re-search-forward "\\w+\\W*" region-end t))
      (1+ (recursive-count-words region-end))
    0))


    
    
	  
	   

(defun count-punctuation-example (beginning end)
  "count punctuation between beginning and end"
  (interactive "r")
  (message "Counting words in region ...")

  ;; 1. set up appropriate conditions.
  (save-excursion
    (goto-char beginning)
    (let ((count 0))
      ;; 2. run the while loop
      ;; and region restriction and search test in while
      (while (and (< (point) end)	
		  (re-search-forward "[.,;:!]" end t))
	(setq count (1+ count)))
      ;; 3. send a message to the user
      (cond ((zerop count)
	     (message "The region does NOT have any punctuation."))
	    ((= count 1)
	     (message "The region has 1 punctuation."))
	    (t
	     (message "The region has %d punctuation." count))))))


;;;;;
.....
,,,,,
!!!!!


(current-time-string)


(defun P-test (&optional arg)
  (interactive "P")
  (if (consp arg)
      (message "consp is true")))


(defun first (lst)
  (interactive)
  (car lst))



(defun rest (lst)
  (interactive)
  (cdr lst))

(first '(a b c d))
(rest '(a b c d))
