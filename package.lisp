(uiop:define-package #:lesspass
  (:export
   #:generate-password
   #:password-profile


   #:site-of
   #:login-of
   #:length-of
   #:counter-of
   #:rules-of

   #:lowercase
   #:uppercase
   #:digits
   #:symbols

   #:*character-rules*)
  (:use #:cl))

(uiop:define-package #:lesspass/test
  (:use #:cl #:lesspass))
