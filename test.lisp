#!/usr/bin/sbcl --script
;; -*- mode: lisp -*-
(require 'asdf)
(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))

#-QUICKLISP
(exit :abort t)

(push '*default-pathname-defaults* asdf:*central-registry*)
(ql:quickload     :lesspass/test)
(asdf:test-system :lesspass/test)
(quit)
