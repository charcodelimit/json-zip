;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
;;; -*- coding: utf-8 -*-

(in-package :cl-user)

(defpackage :json-zip-test-package (:use #:cl #:asdf))
(in-package :json-zip-test-package)

(asdf:oos 'asdf:load-op :asdf-nst)

(defsystem #:json-zip-test
  :class asdf:nst-test-holder
  :description "Unit tests for filtering of tree-like datastructures parsed from JSON-documents"
  :author "Christian Hofmann-Fuchs"
  :version "1.6"
  :license "BSD"
  :serial t
  :nst-packages (:json-zip-tests)
  :depends-on (:json-zip :nst)
  :in-order-to ((asdf:test-op (asdf:load-op :json-zip-test)))
  :components ((:file "tests-json-zip")))
