(defpackage #:diasbruno.configuration
  (:use #:cl)
  (:export
   #:*source*
   #:*destination*
   #:*database*
   #:*opensource*
   #:*markdown-compiler*
   #:is-writing
   #:+iso-8601-format+
   #:+YYYY-MM-DDTHH/MM/SS+
   #:+YYYYMMDD+
   #:+YYYYMMDDTHHMMSS+
   #:+POST-DISPLAY-DATE+))

(in-package #:diasbruno.configuration)

(defvar *env*)
(setf *env* :publishing)

(defvar *source*
  #P "/usr/local/src/site-content/contents/")

(defvar *destination*
  #P "/usr/local/src/diasbruno.github.io")

(defvar *database*
  (merge-pathnames #P"database.json" *source*))

(defvar *opensource*
  (merge-pathnames #P"opensource.json" *source*))

(defvar *markdown-compiler*
  "/home/herospark/.local/bin/comrak")

(defun is-writing ()
  (equal :writing *env*))

(defparameter +iso-8601-time-no-usec-format+
  '((:hour 2) #\: (:min 2) #\: (:sec 2)))

(defparameter +iso-8601-format+
  (append local-time:+iso-8601-date-format+
	  (list #\T)
	  +iso-8601-time-no-usec-format+
	  (list :gmt-offset-or-z)))

(defparameter +YYYY-MM-DDTHH/MM/SS+
  (append local-time:+iso-8601-date-format+
	  (list #\T)
	  '((:hour 2) #\: (:min 2) #\: (:sec 2))
	  (list :gmt-offset-or-z))
  "iso-8601 without microseconds.")

(defparameter +YYYYMMDD+
  '((:year 4) #\/ (:month 2) #\/ (:day 2))
  "RFC-3339 date time, but only date, for file system.")

(defparameter +YYYYMMDDTHHMMSS+
  '((:year 4) (:month 2) (:day 2) (:hour 2) (:min 2) (:sec 2))
  "RFC-3339 date time for file system.")

(defparameter +POST-DISPLAY-DATE+
 '(:short-month #\SPACE (:day 2) #\, #\SPACE (:year 4))
  "Format used to display the date on a post - 'Mon 10, 2023'.")
