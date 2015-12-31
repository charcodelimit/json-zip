;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
;;; -*- coding: utf-8 -*-

(in-package :cl-user)

(defpackage :json-zip-package (:use #:cl #:asdf))
(in-package :json-zip-package)

(defsystem #:json-zip
  :description "Filtering of tree-like datastructures parsed from JSON-documents"
  :author "Christian Hofmann-Fuchs"
  :version "1.6"
  :license "BSD"
  :depends-on ()
  :components ((:file "json-zip")))
