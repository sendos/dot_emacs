;; Adds the new function 'calendar-set-num-months' to the built-in emacs 
;; calendar, which changes the number of months displayed in the calendar
;;
;; Number of months defaults to 3 (as per original calendar) but can be 
;; set to e.g. 6 or 12 months using 'calendar-set-num-months'.
;;
;; Alternatively, you can change the default number of months by editing 
;; the value of 'calendar-num-months' in this file.
;;
;; Load this in your .emacs file to automatically get the new capability
;;
;; Note: Does not yet work well with the '.' (today) function unless 
;; num months is set to 3

(provide 'calendar++)
(require 'calendar)

(defcustom calendar-num-months 3
  "Number of months to show in calendar window."
  :type 'integer
  :version "22.1"
  :group 'calendar)

(defcustom calendar-num-months-per-row 3
  "Number of months to show in each row."
  :type 'integer
  :version "22.1"
  :group 'calendar)

(defvar calendar-num-lines-per-month 9
  "Number of lines each month takes up")

;; ------------------------------------------------------------------------
(defun calendar-set-num-months ()
  "Set number of months to show in calendar window."
  (interactive)
  (setq calendar-num-months (string-to-number (read-string "Number of months to show in calendar window (default 3): " nil nil "3")))
)

;; ------------------------------------------------------------------------
(defun calendar-generate (month year)
  "Generate a three-month Gregorian calendar centered around MONTH, YEAR."
  ;; A negative YEAR is interpreted as BC; -1 being 1 BC, and so on.
  ;; Note that while calendars for years BC could be displayed as it
  ;; stands, almost all other calendar functions (eg holidays) would
  ;; at best have unpredictable results for such dates.
  (if (< (+ month (* 12 (1- year))) 2)
      (error "Months before January, 1 AD cannot be displayed"))
  (setq displayed-month month
        displayed-year year)
  (erase-buffer)
  (calendar-increment-month month year (- 0 (floor (/ (- calendar-num-months 1) 2)))) ;!!!: was -1
  (dotimes (i (+ 1 (* (ceiling (/ calendar-num-months calendar-num-months-per-row)) calendar-num-lines-per-month))) (calendar-ensure-newline)) (goto-line 1)  ;!!!: added
  (dotimes (i calendar-num-months) ;!!!: was 3
    (calendar-generate-month month year
                             (+ calendar-left-margin (* calendar-month-width (mod i calendar-num-months-per-row))) ;!!!: changed i to (mod i calendar-num-months-per-row)
			     (+ 1 (* calendar-num-lines-per-month (floor (/ i calendar-num-months-per-row))))      ;!!!: added
    )
    (calendar-increment-month month year 1)
  )
)

(defun calendar-generate-month (month year indent yoffset) ; !!!: added yoffset
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted at the top of the buffer in which point is currently
located, but indented INDENT spaces.  The indentation is done from the first
character on the line and does not disturb the first INDENT characters on the
line."
  (let ((blank-days                     ; at start of month
         (mod
          (- (calendar-day-of-week (list month 1 year))
             calendar-week-start-day)
          7))
         (last (calendar-last-day-of-month month year))
         (trunc (min calendar-intermonth-spacing
                     (1- calendar-left-margin)))
         (day 1)
         string)
   (goto-char (point-min))
   (goto-line yoffset) ; !!!: added
   (calendar-move-to-column indent)
   (insert
    (calendar-string-spread
     (list (format "%s %d" (calendar-month-name month) year))
     ?\s calendar-month-digit-width))
   (calendar-ensure-newline)
   (calendar-insert-at-column indent calendar-intermonth-header trunc)
   ;; Use the first two characters of each day to head the columns.
   (dotimes (i 7)
     (insert
      (progn
        (setq string
              (calendar-day-name (mod (+ calendar-week-start-day i) 7) nil t))
        (if enable-multibyte-characters
            (truncate-string-to-width string calendar-day-header-width)
          (substring string 0 calendar-day-header-width)))
      (make-string (- calendar-column-width calendar-day-header-width) ?\s)))
   (calendar-ensure-newline)
   (calendar-insert-at-column indent calendar-intermonth-text trunc)
   ;; Add blank days before the first of the month.
   (insert (make-string (* blank-days calendar-column-width) ?\s))
   ;; Put in the days of the month.
   (dotimes (i last)
     (setq day (1+ i))
     ;; TODO should numbers be left-justified, centered...?
     (insert (format (format "%%%dd%%s" calendar-day-digit-width) day
                     (make-string
                      (- calendar-column-width calendar-day-digit-width) ?\s)))
     ;; 'date property prevents intermonth text confusing re-searches.
     ;; (Tried intangible, it did not really work.)
     (set-text-properties
      (- (point) (1+ calendar-day-digit-width)) (1- (point))
      `(mouse-face highlight help-echo ,(eval calendar-date-echo-text)
                   date t))
     (when (and (zerop (mod (+ day blank-days) 7))
                (/= day last))
       (calendar-ensure-newline)
       (setq day (1+ day))              ; first day of next week
       (calendar-insert-at-column indent calendar-intermonth-text trunc)))))
