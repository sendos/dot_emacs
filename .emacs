;; Add MELPA package repository
;; ;;(cond ((>= 24 emacs-major-version)
;;   (require 'package)
;;   (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;          ;;(proto (if no-ssl "http" "https"))
;;          (proto "http")
;;          )
;;
;;     ;; Comment/uncomment next two lines to enable/disable MELPA and MELPA Stable as desired
;;     (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;     ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;  
;;   )
;;   (package-initialize)  
;;   ;;(package-refresh-contents)
;; ;; ) )


;; Set home directory
(setq home-dir "/home/my_username") ; OR (setq home-dir "C:/Users/my_username")

;; Set emacs settings that depend on OS
;;
;; Value of system-type is symbol indicating type of operating system you are using.
;; Special values:      
;;  'gnu'         compiled for a GNU Hurd system.
;;  'gnu/linux'   compiled for a GNU/Linux system.
;;  'darwin'      compiled for Darwin (GNU-Darwin, Mac OS X, ...).
;;  'ms-dos'      compiled as an MS-DOS application.
;;  'windows-nt'  compiled as a native W32 application.
;;  'cygwin'      compiled using the Cygwin library.
;; Anything else indicates some sort of Unix system.
(cond 
      ;; ----------------------------------------
      ;; MS Windows
      ((string-equal system-type "windows-nt")
         ;; Clipboard fixes
         ;;    Revert back to old way of copying/pasting stuff between Emacs and Windows
         ;;    (mainly, copy text to clipboard when selecting in Emacs)
         (setq select-active-regions nil)
         (setq mouse-drag-copy-region t)
         (global-set-key [mouse-2] 'mouse-yank-at-click)
         
         ;; Programs that allow diff'ing buffers
         (setq ediff-diff-program-loc  "C:\\Program Files (x86)\\GnuWin32\\bin\\diff.exe")
         (setq ediff-diff3-program-loc "C:\\Program Files (x86)\\GnuWin32\\bin\\diff3.exe")
       )
      
      ;; ----------------------------------------
      ;; Mac OSX
      ((string-equal system-type "darwin")        
         ;; Put scrollbar on the right 
         (set-scroll-bar-mode 'right) 

         ;; Programs that allow diff'ing buffers
         (setq ediff-diff-program-loc  "diff")
         (setq ediff-diff3-program-loc "diff3")

         ;; Clipboard fixes
         (setq select-active-regions t)
         (setq mouse-drag-copy-region t)
         (global-set-key [mouse-2] 'mouse-yank-at-click)

         ;; Mac-related changes to make Emacs behave a bit more like on other platforms:

         ;; Make the Command key behave like the Control key 
         ;; [Most effective when, in Mac-OS keyboard settings, the CapsLock key is remapped to the CommandKey.
         ;;  This makes the CapsLock key behave like "Windows"-Control in Mac-OS programs (e.g. Control-C) 
         ;;  and, in addition to the code below, makes it behave like regular Control in Emacs (e.g. Control-G)]
         (setq mac-command-modifier 'control)

         ;; Make Fn-UpArrow be "Home" and Fn-DownArrow be "End"
         ;;(global-set-key [prior]    'beginning-of-buffer)
         ;;(global-set-key [next]     'end-of-buffer)
         ;;; The following make sure PageUp and PageDown on keyboards with those keys do the right thing 
         (global-set-key [prior]    'scroll-down)
         (global-set-key [next]     'scroll-up)

         ;; Make OptionKey-UpArrow be PageUp and OptionKey-DownArrow be PageDown
         (global-set-key [M-up]     'scroll-down)
         (global-set-key [M-down]   'scroll-up)

         ;; Redefine C-x C-c to behave like expected
         ;; (i.e. kill the current frame if there are multiple and exit emacs if this is the last frame)
         (defun close-frame-or-kill-emacs () 
           (interactive) 
           (condition-case err 
               (delete-frame)
             (error 
              (if (string= "Attempt to delete the sole visible or iconified frame" (cadr err))
                  (save-buffers-kill-emacs) ;; (kill-emacs)
              )))
          )

         (global-set-key "\C-x\C-c" 'close-frame-or-kill-emacs)

         ;; Set Alt-space (i.e. ESC-space) to set the mark, since C-space doesn't work on Macs
         (global-set-key (kbd "M-SPC")   'set-mark-command)
      ) ;; End Mac OSX settings
      
      ;; ----------------------------------------
      ;; Must be some variant of UNIX/Linux
      (t                                          
         ;; Put scrollbar on the right 
         (set-scroll-bar-mode 'right) 

         ;; Clipboard fixes
         ;;    Fix for clipboard copy/paste on Linux in Emacs 21.2.1 and later
         (setq x-select-enable-clipboard t)
         (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

         ;; Programs that allow diff'ing buffers
         (setq ediff-diff-program-loc  "diff")
         (setq ediff-diff3-program-loc "diff3")
       )
)

;; Emacs IPython Notebook
;(require 'ein)
;(require 'ein-loaddefs)
;(require 'ein-notebook)
;(require 'ein-subpackages)

;; Atomic chrome
;; Extension for Google Chrome browser that allows you to edit text areas of the browser in Emacs
;(require 'atomic-chrome)
;(atomic-chrome-start-server)

;; Set emacs' load path
(setq emacs-dir (expand-file-name (concat home-dir "/.emacsdir")))
(setq load-path (cons emacs-dir load-path))   
 
;; Remove the toolbar
(tool-bar-mode -1)

;; Turn off the beep sound
;(setq visible-bell 1)
;(setq ring-bell-function 'ignore)
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Minor tweaks
(setq inhibit-splash-screen t)     ; Stop splash screen at startup
(setq initial-scratch-message "")  ; Make sure initial scratch buffer is empty

;; Make sure spaces are inserted instead of tabs
(setq-default indent-tabs-mode nil)

;; Use updated calendar
(require 'calendar++)

;; ------------------------------
;; Look and feel of various modes

;; Matlab mode
    (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
      (setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
      (defun my-matlab-mode-hook ()
        (setq matlab-indent-function t) ; if you want function bodies indented
        (setq fill-column 1000)         ; where auto-fill should wrap
        (setq matlab-fill-code nil)     ; stop emacs from auto-filling code as you type
        ;(turn-on-auto-fill)
      )
      (setq matlab-mode-hook 'my-matlab-mode-hook)
      (autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
      (defun my-matlab-shell-mode-hook ()
        '())
      (setq matlab-mode-hook 'my-matlab-mode-hook)


;; Make sure underscore ("_") is treated as part of a word
;; in various modes (so that when you double-click on 
;; a word with an underscore in it, it selects the whole word)
(add-hook 'text-mode-hook   
          '(lambda () (modify-syntax-entry ?_ "w" text-mode-syntax-table)))
(add-hook 'makefile-mode-hook   
          '(lambda () (modify-syntax-entry ?_ "w" makefile-mode-syntax-table)))
(add-hook 'cfg-mode-hook   
          '(lambda () (modify-syntax-entry ?_ "w" cfg-mode-syntax-table)))
(add-hook 'awk-mode-hook   
          '(lambda () (modify-syntax-entry ?_ "w" awk-mode-syntax-table)))
(add-hook 'perl-mode-hook   
          '(lambda () (modify-syntax-entry ?_ "w" perl-mode-syntax-table)))
(add-hook 'python-mode-hook   
          '(lambda () (modify-syntax-entry ?_ "w" python-mode-syntax-table)))
(add-hook 'c-mode-hook   
          '(lambda () (modify-syntax-entry ?_ "w" c-mode-syntax-table)))
(add-hook 'c++-mode-hook   
          '(lambda () (modify-syntax-entry ?_ "w" c++-mode-syntax-table)))
(add-hook 'matlab-mode-hook   
          '(lambda () (modify-syntax-entry ?_ "w" matlab-mode-syntax-table)))

;; Make the braces under if and for statements
;; appear with no additional indentation
(setq c-default-style "stroustrup")
(add-hook 'c-mode-hook      '(lambda () (setq c-basic-offset 2)))          
(add-hook 'c++-mode-hook    '(lambda () (setq c-basic-offset 2)))
(add-hook 'awk-mode-hook    '(lambda () (setq c-basic-offset 2)))
(add-hook 'php-mode-hook    '(lambda () (setq c-basic-offset 2)))
(add-hook 'perl-mode-hook   '(lambda () (setq perl-indent-level 2)))
;;(add-hook 'matlab-mode-hook '(lambda () (setq matlab-indent-level 2))) ;; Needed???


;; Make sure the TAB indents in C and C++ files
;; (which was broken on Linux machines, Emacs 21.2.1)
;(add-hook 'c-mode-hook   '(lambda () (local-set-key [tab] 'c-indent-command)))
;(add-hook 'c++-mode-hook '(lambda () (local-set-key [tab] 'c-indent-command)))


;; Make sure .h files are in C++ mode
  (setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))

;; Make sure .cshrc files are in sh-mode 
  (setq auto-mode-alist (cons '("\\.cshrc" . sh-mode) auto-mode-alist))

;; Make sure files containing 'makefile' in their name are in sh-mode 
  (setq auto-mode-alist (cons '("\\(M\\|m\\)akefile" . makefile-mode) auto-mode-alist))

;; Make sure .php files are in PHP mode
  (autoload 'php-mode "php-mode" "Enter PHP mode." t)
  (setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))

;; Multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script[^>]*>" "</script>")
                  (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;;; Miscellaneous bindings
(global-set-key [f1]    'goto-line)
(global-set-key [f2]    'replace-string)
(global-set-key [f3]    'query-replace)
(global-set-key [C-f3]  'query-replace-regexp)

;(global-set-key [f4]    (lambda () (interactive) (load-file (concat home-dir "/.emacs"))))
;(global-set-key [f4]    (lambda () (interactive) (set-selective-display (if selective-display nil 3))))
;(global-set-key [f5]    'matlab-shell-save-and-go) ; Only useful in matlab .m files and if Matlab is available
(global-set-key [f5]    'search-buffers)
(global-set-key [C-f5]  'search-buffers-for-current-word)

(global-set-key [f6]    'ediff-revision)
(global-set-key [f7]    'ediff-buffers)
(global-set-key [C-f7]  'ediff-regions-this-buffer)

(global-set-key [f9]    'switch-to-buffer)
(global-set-key [f10]   'next-buffer)
(global-set-key [C-f10] 'previous-buffer)

(global-set-key [f11]   'dts-switch-between-header-and-source)
(global-set-key [f12]   'prev-buffer)
(global-set-key [C-f12] 'goto-function-under-cursor)

(global-set-key [home]  'beginning-of-buffer)
(global-set-key [end]   'end-of-buffer)

; Make control-pageup and control-pagedown be Home and End
(global-set-key [C-prior] 'beginning-of-buffer)
(global-set-key [C-next]  'end-of-buffer)

;; Bind kill-rectangle and yank-rectangle to easier keystrokes (Ctrl-Shift-k and Ctrl-Shift-y)
(global-set-key (kbd "C-S-k") 'kill-rectangle)
(global-set-key (kbd "C-S-y") 'yank-rectangle)

; Make control-. (control-period) and control-' (control-quote) expand variable names
(global-set-key [(control .)]   'dabbrev-expand)
(global-set-key [(control \')]  'dabbrev-expand)

;; Use Ctrl-b instead of 'Ctrl-x b' to switch to buffer
;; and Ctrl-o instead of 'Ctrl-x o' to switch to other buffer
(global-set-key [(control b)]  'switch-to-buffer)
(global-set-key [(control o)]  'other-window)

;; Use Ctrl-<N>, Ctrl-x <N>, for N equal to 0, 1, or 2
(global-set-key (kbd "C-0")  'delete-window)
(global-set-key (kbd "C-1")  'delete-other-windows)
(global-set-key (kbd "C-2")  'split-window-below)

; In Python: Make Ctrl-TAB       and Ctrl-> indent a block to the right
;            and  Ctrl-Shift-TAB and Ctrl-< indent a block to the left
(global-set-key [C-tab]            'python-indent-shift-right)
(global-set-key [C-S-iso-lefttab]  'python-indent-shift-left)
(global-set-key (kbd "C->")        'python-indent-shift-right)
(global-set-key (kbd "C-<")        'python-indent-shift-left)


(global-set-key (kbd "C-c C-p")  'python-mode)
(global-set-key (kbd "C-c C-s")  'sql-mode)


;;Define the mouse scroll wheel
(defun up-slightly   () (interactive) (scroll-up   5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [wheel-down]        'up-slightly)
(global-set-key [wheel-up]          'down-slightly)
(global-set-key [double-wheel-down] 'up-slightly)
(global-set-key [double-wheel-up]   'down-slightly)
(global-set-key [triple-wheel-down] 'up-slightly)
(global-set-key [triple-wheel-up]   'down-slightly)


;; Tramp mode
;(setq tramp-default-method "ssh")
;C-x C-f /ssh:__username__@__servername__:__path_to_folder__
;(defun tramp_connect_to_server()
;  (interactive)
;  (setq tramp-default-method "ssh")
;  (find-file "/ssh:__username__@__servername__:__path_to_folder__")
;)


;; ------------------------
;; Org-mode stuff
(setq org-cycle-include-plain-lists t)

;; Variables set automatically by emacs
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ediff-diff-options "-w -B")
 '(ediff-diff-program  ediff-diff-program-loc)
 '(ediff-diff3-program ediff-diff-program-loc)
 '(matlab-indent-level 2)
 '(ns-use-qd-smoothing t)
 ;'(package-selected-packages (quote (php-mode ein kmb atomic-chrome)))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t) 
 ;; AUCTeX variables
 ;'(LaTeX-command "latex")
 ;'(TeX-PDF-mode t)
 ;'(font-latex-fontify-script nil)
)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; find occurrences of variable settings that don't end in ;
; (isearch-forward-regexp '^[^%;]+=[^%;]+$')

;; ------------------------------------------------------------------------
(defun dts-switch-between-header-and-source ()
  "Switch between a c/c++ header (.h) and its corresponding source (.c/.cpp)."
  (interactive)
  ;; grab the base of the current buffer's file name
  (setq bse (file-name-sans-extension buffer-file-name))
  ;; and the extension, converted to lowercase so we can
  ;; compare it to "h", "c", "cpp", etc
  (setq ext (downcase (file-name-extension buffer-file-name)))
  ;; This is like a c/c++ switch statement, except that the
  ;; conditions can be any true/false evaluated statement
  (cond
   ;; first condition - the extension is "h"
   ((equal ext "h")
    ;; first, look for bse.c 
    (setq nfn (concat bse ".c"))
    (if (file-exists-p nfn)
        ;; if it exists, either switch to an already-open
        ;;  buffer containing that file, or open it in a new buffer
        (find-file nfn)
      ;; this is the "else" part - note that if it is more than
      ;;  one line, it needs to be wrapped in a (progn )
      (progn
        ;; look for a bse.cpp
        (setq nfn (concat bse ".cpp"))
        ;; likewise 
        (find-file nfn)
        )
      )
    )
   ;; second condition - the extension is "c" or "cpp"
   ((or (equal ext "cpp") (equal ext "c"))
    ;; look for a corresponding bse.h
    (setq nfn (concat bse ".h"))
    (find-file nfn)
    )
   )
  )

;; ------------------------------------------------------------------------
(defun get-current-word ()
  "Return the word under the cursor (i.e. under the current 'point')"
  (interactive)
  (thing-at-point 'word)
)

;; ------------------------------------------------------------------------
(defun goto-function-under-cursor ()
  "Open the function under the cursor"
  (interactive)
  (setq current-word (get-current-word))
  (setq filename (concat current-word ".m"))
  (if (equal major-mode 'matlab-mode)
      (if (file-exists-p filename)
            (find-file filename)
            (message "Could not find a file called %s" filename)
      )
      (message "Not in Matlab mode")
  )
)

;; ------------------------------------------------------------------------
(defun prev-buffer ()
  "Switch to the prev buffer"
  (interactive)
  (switch-to-buffer (other-buffer))
)


;; ------------------------------------------------------------------------
(defun ediff-regions-this-buffer ()
  "Diff regions in current buffer"
  (interactive)
  (ediff-regions-linewise (current-buffer) (current-buffer))
)


;; ------------------------------------------------------------------------
(defun insert-sequence ()
  "Insert sequence of numbers"
  (interactive)
  (setq num1 (string-to-number (read-string "Number to start the sequence (default 1): " nil nil "1")))
  (setq num2 (string-to-number (read-string "Number to end the sequence (default 10): " nil nil "10")))
  (dotimes (i (1+ (- num2 num1))) (insert (format "%2d\n" (+ num1 i))))
)

;; ------------------------------------------------------------------------
(defun uniq-lines (beg end)
  "Unique lines in region.
  Called from a program, there are two arguments:
  BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))


;; ------------------------------------------------------------------------
(defun search-buffers ()
  "Search all open buffers"
  (interactive)
  (setq regex (read-string "Regex to search for: " nil nil "."))
  (multi-occur-in-matching-buffers "." regex)
)

(defun search-buffers-for-current-word ()
  "Search all open buffers for word under cursor"
  (interactive)
  (setq current-word (get-current-word))
  (multi-occur-in-matching-buffers "." current-word)
)

