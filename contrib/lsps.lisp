(require      :asdf)
(require      :uiop)
(ql:quickload :cl-csv)
(ql:quickload :lesspass)
(ql:quickload :trivia)
(ql:quickload :clingon)

(defpackage :lsps (:use :cl))
(in-package :lsps)

;; FIXME: handle errors gracefully...

;; FIXME: allow for the master password to have its sha512 sum checked...

(defun insert (value target-lst n)
  "Insert VALUE in TARGET-LST at position N."
  (append (subseq target-lst 0 n)
          (list value)
          (subseq target-lst n)))

(defun read-config ()
  (cl-csv:read-csv
   (merge-pathnames
    (concatenate 'string (namestring (uiop:xdg-config-home)) "/lsps/sites"))
   :separator #\;))

(defun rules-str-to-rules-lst (rules-str)
  (loop :for char :in (coerce (string-downcase rules-str) 'list)
        :collect
        (cond
          ((char= char #\l) 'lesspass:lowercase)
          ((char= char #\u) 'lesspass:uppercase)
          ((char= char #\d) 'lesspass:digits)
          ((char= char #\s) 'lesspass:symbols)
          (t nil))))

(defun lst-to-password-prof (lst)
  (assert (= (length lst) 5))
  (trivia:match lst
    ((list site login length rules counter)
     (make-instance
      'lesspass:password-profile
      :login   login
      :site    site
      :length  (parse-integer length)
      :counter (parse-integer counter)
      :rules   (rules-str-to-rules-lst rules)))))

(defun prompt-for-input (prompt-string &optional input-stream password-p)
  (let ((cmd
          (concatenate
           'string
           "dmenu"
           (if password-p " -P " "")
           " -p '" prompt-string "'"
           " -l 10 ")))
    (string-trim
     '(#\Newline)
     (uiop:run-program cmd :input input-stream :output :string))))

(defun prompt-for-site (lsps-db)
  (prompt-for-input "Choose Site:"
                    (make-string-input-stream
                     (format nil "~{~A~%~}" (mapcar #'car lsps-db)))))

(defun prompt-for-mpw ()
  (prompt-for-input "Enter your master password:" nil t))

(defun prompt-for-login ()
  (prompt-for-input "Enter the login:"))

(defun prompt-for-length ()
  (prompt-for-input "Enter the password length:"))

(defun prompt-for-counter ()
  (prompt-for-input "Enter the password count:"))

(defun prompt-for-rules ()
  (prompt-for-input "Enter the password rules:"))

(defun cli-options ()
  (list
   (clingon:make-option
    :string
    :description "login for the password"
    :short-name #\l
    :long-name "login"
    :initial-value ""
    :key :login)
   (clingon:make-option
    :string
    :description "site for the password"
    :short-name #\s
    :long-name "site"
    :initial-value ""
    :key :site)
   (clingon:make-option
    :string
    :description "masterpassword for the password"
    :short-name #\m
    :long-name "masterpassword"
    :initial-value ""
    :key :masterpassword)
   (clingon:make-option
    :string
    :description "rules for the password"
    :short-name #\r
    :long-name "rules"
    :initial-value "luds"
    :key :rules)
   (clingon:make-option
    :string
    :description "counter for the password"
    :short-name #\c
    :long-name "counter"
    :initial-value "1"
    :key :counter)
   (clingon:make-option
    :string
    :description "length for the password"
    :short-name #\n
    :long-name "length"
    :initial-value "16"
    :key :length)))

(defun cli-handler (cmd)
  (let ((lsps-db            (read-config))
        (cli-login          (clingon:getopt cmd :login))
        (cli-site           (clingon:getopt cmd :site))
        (cli-masterpassword (clingon:getopt cmd :masterpassword))
        (cli-counter        (clingon:getopt cmd :counter))
        (cli-rules          (clingon:getopt cmd :rules))
        (cli-length         (clingon:getopt cmd :length)))
    (let* ((site
             (if (not (string= "" cli-site)) cli-site
                 (prompt-for-site lsps-db)))
           (login
             (if (not (string= "" cli-login)) cli-login
                 (prompt-for-login)))
           (masterpassword
             (if (not (string= "" cli-masterpassword)) cli-masterpassword
                 (prompt-for-mpw))))

      ;; FIXME: properly handle the string here...
      ;; IE: allow for copying to the clipboard etc.
      (princ
       (lesspass:generate-password
        (lst-to-password-prof
         (let ((site-is-in-lsps-db-p
                 (find-if (lambda (it) (string= (car it) site)) lsps-db)))
           (if site-is-in-lsps-db-p
               (insert login site-is-in-lsps-db-p 1)
               (list
                site
                login
                (if (not (string= "" cli-length)) cli-length
                    (prompt-for-length))
                cli-rules
                (if (not (string= "" cli-counter)) cli-counter
                    (prompt-for-counter))))))
        masterpassword)))))

(defun cli-command ()
  (clingon:make-command
   :name "lsps"
   :version "1.0.0"
   :options (cli-options)
   :handler #'cli-handler))

(defparameter *app* (cli-command))

(defun main () (clingon:run *app*))
