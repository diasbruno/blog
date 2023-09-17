(defpackage #:diasbruno.configuration
  (:use #:cl)
  (:export
   #:*markdown-compiler*
   #:is-writing
   #:+iso-8601-format+
   #:+YYYY-MM-DDTHH/MM/SS+
   #:+YYYYMMDD+
   #:+YYYYMMDDTHHMMSS+
   #:+POST-DISPLAY-DATE+
   #:logging
   #:environment
   #:post-contents-path
   #:opensource-json
   #:markdown-compiler
   #:switch-profile
   #:destination-path))

(in-package #:diasbruno.configuration)

(defvar *default-content-path* #P "~/Programming/site/contents/")

(chameleon:defconfig
  (logging :debug
           "log4cl log level.")
  (environment :publishing
               "what we are doing?")
  (post-contents-path *default-content-path*
                      "Path where are all markdown files?")
  (destination-path #P"~/Programming/diasbruno.github.io/"
                    "Where to write the files.")
  (opensource-json (merge-pathnames #P"database.json" *default-content-path*)
                   "JSON file containing the list of opensource projects.")
  (markdown-compiler "comrak"
                     "Markdown compiler."))

(chameleon:defprofile :default)

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
