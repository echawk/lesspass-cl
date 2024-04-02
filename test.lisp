#!/usr/bin/sbcl --script
;; -*- mode: lisp -*-
(require 'asdf)
(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))

(push '*default-pathname-defaults* asdf:*central-registry*)
(ql:quickload     :lesspass/test)
(asdf:test-system :lesspass/test)
(quit)
