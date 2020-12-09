(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (manoj-dark)))
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(fci-rule-color "#37474f")
 '(font-use-system-font t)
 '(hl-sexp-background-color "#1c1f26")
 '(inhibit-startup-screen t)
 '(major-mode (quote org-mode))
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages (quote (pdf-tools material-theme elpy citeproc)))
 '(python-shell-interpreter "/home/hack/anaconda3/bin/python")
 '(scroll-bar-mode nil)
 '(sentence-end "[.?!]")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil)
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; package repository: melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; global setting

;; upcase region
(put 'upcase-region 'disabled nil)
;; Make sure spaces are used when indenting code
(setq-default indent-tabs-mode nil)
;; tabs
(setq-default tab-width 4)
;; common lisp environment
(setq inferior-lisp-program "/usr/local/bin/sbcl") 

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)

;; line no.
(global-linum-mode 1)
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for Org global command bounds
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)


;; macro
(load-file "~/.emacs.d/macro")
(global-set-key "\C-x\C-kp" 'python-src)
(global-set-key "\C-x\C-ki" 'inline-image)
(global-set-key "\C-x\C-ka" 'example)
(global-set-key "\C-x\C-kq" 'equation)
(global-set-key "\M-n" 'math-inline)
(global-set-key "\C-x\C-kj" 'java-src)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; html helper mode
;; (setq load-path (cons "~/elisp/html-helper" load-path))
;; ;; load html helper mode
;; (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;; ;; edit html file with html helper mode be default
;; (setq auto-mode-alist (cons '("\\.html?$" . html-helper-mode)
;; 							auto-mode-alist))
;; ;; address
;; (setq html-helper-address-string "<a href=\"mailto:chyson@aliyun.com\">Hack Chyson</a>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org mode

(setq org-tag-persistent-alist '((:startgroup . nil)
                                 ("@work" . ?w) ("@home" . ?h)
                                 ("@outside" . ?o)
                                 (:endgroup . nil)))

;; Turn the state to done when all the subtasks is marked done automatically
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


;; When you quit the agenda view, restore the original window.
(setq org-agenda-restore-windows-after-quit t)

;; In agenda view, when you move the cursor between entires,
;; the other window will show the corresponding orginal file's content.
(setq org-agenda-start-with-follow-mode t)

;; do not show the sublevel todo tasks in sparse tree or agenda view.
(setq org-agenda-todo-list-sublevels nil)

;; When cursor is in the position where this entry is folded,
;; show the entry with the cursor.
(setq org-catch-invisible-edits (quote show))

;; When you walk away and the clock is working on,
;; Emacs will ask you how long the idle time should substracted
;; from the clocking time.
(setq org-clock-idle-time 10)

;; default format in column view
(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %SCHEDULED %DEADLINE")

;; Three lines under the entry will belong to the heading above.
;; When showing the structure of the tree, this three lines will be
;; folded into the headings.
(setq org-cycle-separator-lines 4)      

;; The parent task will not be marted done
;; when its subtasks have undone state.
(setq org-enforce-todo-dependencies t)

;; when exporting to pdf, html and so on,
;; The line breaks will be keeped.
(setq org-export-preserve-breaks t)

;; When exporting out, a^1 will be exactly a^1,
;; but the 1 in the a^{1} will be transferred into superscript.
;; Same to the subscript.
(setq org-export-with-sub-superscripts (quote {}))

;; Do not record the timestamp when the task is marked done
;; because I record them in org-todo-keywords setting.
(setq org-log-done nil)

;; move all loggnig information into a drawer, LOGBOOK
(setq org-log-into-drawer t)

;; loaed modules in org mode
(setq org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))


;; todo keywords, fast accesses, record the timestamp
(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                          (type "USECASE(u)" "SUMMARY(s)" "LEARN(l)" "|" "DONE(o)"))) 
      


;; Do not include diary into the agenda view.
(setq org-agenda-include-diary nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maximize at startup
(toggle-frame-maximized)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; abbrevation
(setq-default abbrev-mode t)
(read-abbrev-file "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
