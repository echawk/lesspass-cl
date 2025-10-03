;; Lesspass v2 implementation in Common Lisp.
;; Should be useful to someone. (Beyond myself of course)

;; Other implementations:
;; https://github.com/bannerets/hlesspass
;; https://github.com/tuxlinuxien/lesspassgo
;; https://github.com/lesspass/lesspass

(in-package #:lesspass)

(defconstant +iterations+ 100000)

(defvar *character-rules*
  '((lowercase . "abcdefghijklmnopqrstuvwxyz")
    (uppercase . "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    (digits    . "0123456789")
    (symbols   . "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")))

(defclass password-profile ()
  ((site    :initarg :site    :accessor site-of    :type string)
   (login   :initarg :login   :accessor login-of   :type string)
   (length  :initarg :length  :accessor length-of  :type integer)
   (counter :initarg :counter :accessor counter-of :type integer)
   (rules   :initarg :rules   :accessor rules-of   :type list))
  (:documentation
   "Store of all required data for lesspass password derivation."))

(defun calculate-entropy (password-profile masterpassword)
  "Return the entropy (integer) value that will be used to generate the rest
of the password. Uses the site, login, and counter slots of PASSWORD-PROFILE
as a salt, with the MASTERPASSWORD being hashed."
  (declare (type string masterpassword))
  (let ((salt
          (concatenate
           'string
           (site-of password-profile)
           (login-of password-profile)
           (string-downcase
            (write-to-string
             (counter-of password-profile) :base 16)))))
    (parse-integer
     (ironclad:byte-array-to-hex-string
      (ironclad:pbkdf2-hash-password
       (babel:string-to-octets masterpassword)
       :salt (babel:string-to-octets salt)
       :digest 'ironclad:sha256
       :iterations +iterations+))
     :radix 16)))

(defmacro divmod (n m) `(multiple-value-bind (q r) (floor ,n ,m) (list q r)))

(defun consume-entropy (generated-password quotient set-of-chars max-length)
  (if (>= (length generated-password) max-length)
      (list generated-password quotient)
      (let* ((qr (divmod quotient (length set-of-chars)))
             (q  (first  qr))
             (r  (second qr)))
        (consume-entropy
         (concatenate 'string generated-password
                      (string (char set-of-chars r)))
         q
         set-of-chars
         max-length))))

(defun order-rules (a b)
  "Given two rule symbols A & B, return T if A is before B in *CHARACTER-RULES*.
NIL otherwise."
  (let ((car-char-rules (mapcar #'car *character-rules*)))
    (< (position a car-char-rules)
       (position b car-char-rules))))

(defun rules-to-charset (rules)
  "Converts a list of lesspass rule symbols, RULES, into a string suitable
for the computation of a password."
  (let ((sorted-rules
          (sort rules #'order-rules)))
    (apply #'concatenate 'string
           (mapcar (lambda (S) (rest (assoc S *character-rules*)))
                   sorted-rules))))

(defun get-one-char-per-rule (entropy rules)
  (let ((ent entropy)
        (one-char-per-rules ""))
    (loop :for rule :in rules
          :do
             (let* ((avail-chars   (rules-to-charset (list rule)))
                    (value-entropy (consume-entropy "" ent avail-chars 1))
                    (value (first  value-entropy))
                    (en    (second value-entropy)))
               (setf ent en)
               (setf one-char-per-rules
                     (concatenate 'string one-char-per-rules value))))
    (list one-char-per-rules ent)))

(defun insert-string-psuedo-randomly (password entropy string)
  (let ((pass password)
        (entr entropy))
    (loop :for char :in (coerce string 'list)
          :do
             (let* ((qr (divmod entr (length pass)))
                    (q  (first  qr))
                    (r  (second qr)))
               (setf pass
                     (concatenate 'string
                                  (subseq pass 0 r)
                                  (string char)
                                  (subseq pass r (length pass))))
               (setf entr q)))
    pass))

(defun render-password (entropy password-profile)
  (let* ((rules (sort (rules-of password-profile) #'order-rules))

         (set-of-chars (rules-to-charset rules))
         (passwd--passwd-entropy (consume-entropy
                                  "" entropy set-of-chars
                                  (- (length-of password-profile)
                                     (length rules))))

         (passwd         (first  passwd--passwd-entropy))
         (passwd-entropy (second passwd--passwd-entropy))

         (chars-to-add--char-entropy (get-one-char-per-rule
                                      passwd-entropy rules))

         (chars-to-add (first  chars-to-add--char-entropy))
         (char-entropy (second chars-to-add--char-entropy))

         (pass (insert-string-psuedo-randomly
                passwd char-entropy chars-to-add)))
    pass))

(defun generate-password (password-profile masterpassword)
  "Return a string containing the generated password given the PASSWORD-PROFILE
and the MASTERPASSWORD."
  (let ((entropy (calculate-entropy password-profile masterpassword)))
    (render-password entropy password-profile)))
