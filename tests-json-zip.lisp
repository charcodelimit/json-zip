;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: ; Base: 10 -*-
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        tests-json-zip.lisp
;;; LANGUAGE:    Common-Lisp
;;;
;;; DESCRIPTION
;;;  Unit Tests for json-zip
;;;
;;;  (nst:nst-cmd :run-package #:json-zip-tests)
;;;
;;; Author: Christian Hofmann-Fuchs
;;;
;;; Created: Mo Sep  7 20:16:58 2015 (+0200)
;;;
;;; Last-Updated: Mo Jul 13 21:20:18 2020 (+0200)
;;;           By: Christian Hofmann-Fuchs
;;;           Update #: 902
;;;
;;; Copyright (C) 2015, Christian Hofmann-Fuchs. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILIpTY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;
;;;****************************************************************************

(in-package :cl-user)
(defpackage #:json-zip-tests
  (:documentation "Zipper test suites")
  (:use :cl :threading :zipper :zipper-enumeration :nst))
(in-package #:json-zip-tests)

(nst:def-fixtures tree-fixture
    (:documentation "A simple tree in list form")
  (test-tree '(:foo (:bar :baz (:bork)))))

;;; THREADING - BASICS

(nst:def-test-group test-threading (tree-fixture)
  (:documentation "Sequentially apply functions to the result of the previously applied function, starting with an initial form.")
  ;; no location, tree only
  (nst:def-test test-threading-independently
      (:equal '(:bar :baz (:bork)))
    (-> test-tree (lambda (x) (member :foo x)) (lambda (x) (cadr x))))
  ;; location, no access or movement functions used
  (nst:def-test test-threading-with-location
      (:equal '(:bar :baz (:bork)))
    (-> (make-location test-tree)
      (lambda (x) (car x)) ;; retrieve tree
      (lambda (x) (member :foo x))
      (lambda (x) (cadr x)))))

;;; ZIPPER

(nst:def-test-group test-zipper (tree-fixture)
  (:documentation "Test zipper functions using tree (:foo (:bar :baz (:bork)))")
  (nst:def-test test-basic-movement-1
      (:symbol :foo)
    (subtree (down (make-location test-tree))))
  (nst:def-test test-basic-movement-2
      (:symbol :bar)
    (subtree (down (right (down (make-location test-tree))))))
  (nst:def-test test-basic-movement-3
      (:symbol :bar)
    (subtree (left (right (down (right (down (make-location test-tree))))))))
  (nst:def-test test-threading-and-movement-1
      (:symbol :foo)
    (-> (make-location test-tree) #'down #'subtree))
  (nst:def-test test-threading-and-movement-2
      (:symbol :bork)
    (-> (make-location test-tree) #'down #'right #'down #'right #'right #'down #'subtree))
  (nst:def-test branchp-positive
      :true (zipper::branch-p (-> (make-location test-tree) #'down #'right)))
  (nst:def-test branchp-negative
      :true (not (zipper::branch-p (-> (make-location test-tree) #'down)))))

(nst:def-test-group test-location (tree-fixture)
  (:documentation "Test functions related to the location in the tree"))

;; pointer at :bar
;; '(:foo (:bar :baz (:bork)))
;;         |
(nst:def-test (test-right-locations :group test-location)
 (:equal '(:bar :baz (:bork)))
 (map 'list
      (lambda (x) (car x))
      (right-locs (-> (make-location test-tree) #'down #'right #'down) (make-hash-table))))


;; pointer at (:bork)
;; '(:foo (:bar :baz (:bork)))
;;                   |
(nst:def-test (test-left-locations :group test-location)
 (:equal '((:bork) :baz :bar))
  (map 'list
       (lambda (x) (car x))
       (left-locs (-> (make-location test-tree) #'down #'right #'down #'right #'right) (make-hash-table))))

(nst:def-test (test-rightmost-p-positive :group test-location)
    :true (rightmost-p
           (-> (make-location test-tree) #'down #'right #'down #'right #'right)))

(nst:def-test (test-rightmost-p-negative :group test-location)
    :true (not (leftmost-p
                (-> (make-location test-tree) #'down #'right #'down #'right #'right))))

(nst:def-test (test-leftmost-p-positive :group test-location)
    :true (leftmost-p
           (-> (make-location test-tree) #'down #'right #'down)))

(nst:def-test (test-leftmost-p-negative :group test-location)
    :true (not (rightmost-p
                (-> (make-location test-tree) #'down #'right #'down))))


;;; ZIPPER-ENUMERATION

#|
{\"name\":\"Default colors\",
 \"colorsArray\": [
    {\"hexValue\": \"#f00\", \"colorName\": \"red\"},
    {\"hexValue\": \"#0f0\", \"colorName\": \"green\"},
    {\"hexValue\": \"#00f\",\"colorName\": \"blue\"},
    {\"hexValue\": \"#0ff\",\"colorName\": \"cyan\"},
    {\"hexValue\": \"#f0f\",\"colorName\": \"magenta\"},
    {\"hexValue\": \"#ff0\",\"colorName\": \"yellow\"},
    {\"hexValue\": \"#000\",\"colorName\": \"black\"}
 ]}
|#
(defvar *default-colors-json*
  '((:NAME . "Default Colors")
    (:COLORS-ARRAY
     ((:HEX-VALUE . "#f00") (:COLOR-NAME . "red"))
     ((:HEX-VALUE . "#0f0") (:COLOR-NAME . "green"))
     ((:HEX-VALUE . "#00f") (:COLOR-NAME . "blue"))
     ((:HEX-VALUE . "#0ff") (:COLOR-NAME . "cyan"))
     ((:HEX-VALUE . "#f0f") (:COLOR-NAME . "magenta"))
     ((:HEX-VALUE . "#ff0") (:COLOR-NAME . "yellow"))
     ((:HEX-VALUE . "#000") (:COLOR-NAME . "black")))))

#|
[[{"id":"1","filename":"node.js","url":"http:\/\/nodejs.org"},
  {"id":"2","filename":"github.atom","url":"http:\/\/planet.lisp.org"}],
 {"version":"1.0"}
]
|#
(defvar *page-links*
  '((((:ID . "1") (:FILENAME . "node.js")
      (:URL . "http://nodejs.org"))
     ((:ID . "2") (:FILENAME . "github.atom")
      (:URL . "http://planet.lisp.org")))
    ((:VERSION . "1.0"))))

#|
{ "sizes": { "canblog": 0, "canprint": 0, "candownload": 1,
"size": [
  { "label": "Square", "width": 75, "height": 75, "source": "http:\/\/farm7.static.flickr.com\/6068\/6115633659_500f8bbd74_s.jpg", "url": "http:\/\/www.flickr.com\/photos\/b34u_h4r13y_n47h4n_7h0m45\/6115633659\/sizes\/sq\/", "media": "photo" },
  { "label": "Thumbnail", "width": 100, "height": 66, "source": "http:\/\/farm7.static.flickr.com\/6068\/6115633659_500f8bbd74_t.jpg", "url": "http:\/\/www.flickr.com\/photos\/b34u_h4r13y_n47h4n_7h0m45\/6115633659\/sizes\/t\/", "media": "photo" },
  { "label": "Small", "width": "240", "height": "159", "source": "http:\/\/farm7.static.flickr.com\/6068\/6115633659_500f8bbd74_m.jpg", "url": "http:\/\/www.flickr.com\/photos\/b34u_h4r13y_n47h4n_7h0m45\/6115633659\/sizes\/s\/", "media": "photo" },
  { "label": "Medium", "width": "500", "height": "331", "source": "http:\/\/farm7.static.flickr.com\/6068\/6115633659_500f8bbd74.jpg", "url": "http:\/\/www.flickr.com\/photos\/b34u_h4r13y_n47h4n_7h0m45\/6115633659\/sizes\/m\/", "media": "photo" },
  { "label": "Medium 640", "width": "640", "height": "424", "source": "http:\/\/farm7.static.flickr.com\/6068\/6115633659_500f8bbd74_z.jpg", "url": "http:\/\/www.flickr.com\/photos\/b34u_h4r13y_n47h4n_7h0m45\/6115633659\/sizes\/z\/", "media": "photo" },
  { "label": "Large", "width": "1024", "height": "678", "source": "http:\/\/farm7.static.flickr.com\/6068\/6115633659_500f8bbd74_b.jpg", "url": "http:\/\/www.flickr.com\/photos\/b34u_h4r13y_n47h4n_7h0m45\/6115633659\/sizes\/l\/", "media": "photo" },
  { "label": "Original", "width": "1440", "height": "954", "source": "http:\/\/farm7.static.flickr.com\/6068\/6115633659_cdaca906e1_o.jpg", "url": "http:\/\/www.flickr.com\/photos\/b34u_h4r13y_n47h4n_7h0m45\/6115633659\/sizes\/o\/", "media": "photo" }
] }, "stat": "ok" }
|#
(defvar *flickr-json*
  '((:SIZES
     (:CANBLOG . 0)
     (:CANPRINT . 0)
     (:CANDOWNLOAD . 1)
     (:SIZE
      ((:LABEL . "Square") (:WIDTH . 75) (:HEIGHT . 75)
       (:SOURCE . "http://farm7.static.flickr.com/6068/6115633659_500f8bbd74_s.jpg")
       (:URL . "http://www.flickr.com/photos/b34u_h4r13y_n47h4n_7h0m45/6115633659/sizes/sq/")
       (:MEDIA . "photo"))
      ((:LABEL . "Thumbnail") (:WIDTH . 100) (:HEIGHT . 66)
       (:SOURCE . "http://farm7.static.flickr.com/6068/6115633659_500f8bbd74_t.jpg")
       (:URL . "http://www.flickr.com/photos/b34u_h4r13y_n47h4n_7h0m45/6115633659/sizes/t/")
       (:MEDIA . "photo"))
      ((:LABEL . "Small") (:WIDTH . "240") (:HEIGHT . "159")
       (:SOURCE . "http://farm7.static.flickr.com/6068/6115633659_500f8bbd74_m.jpg")
       (:URL . "http://www.flickr.com/photos/b34u_h4r13y_n47h4n_7h0m45/6115633659/sizes/s/")
       (:MEDIA . "photo"))
      ((:LABEL . "Medium") (:WIDTH . "500") (:HEIGHT . "331")
       (:SOURCE . "http://farm7.static.flickr.com/6068/6115633659_500f8bbd74.jpg")
       (:URL . "http://www.flickr.com/photos/b34u_h4r13y_n47h4n_7h0m45/6115633659/sizes/m/")
       (:MEDIA . "photo"))
      ((:LABEL . "Medium 640") (:WIDTH . "640") (:HEIGHT . "424")
       (:SOURCE . "http://farm7.static.flickr.com/6068/6115633659_500f8bbd74_z.jpg")
       (:URL . "http://www.flickr.com/photos/b34u_h4r13y_n47h4n_7h0m45/6115633659/sizes/z/")
       (:MEDIA . "photo"))
      ((:LABEL . "Large") (:WIDTH . "1024") (:HEIGHT . "678")
       (:SOURCE . "http://farm7.static.flickr.com/6068/6115633659_500f8bbd74_b.jpg")
       (:URL . "http://www.flickr.com/photos/b34u_h4r13y_n47h4n_7h0m45/6115633659/sizes/l/")
       (:MEDIA . "photo"))
      ((:LABEL . "Original") (:WIDTH . "1440") (:HEIGHT . "954")
       (:SOURCE . "http://farm7.static.flickr.com/6068/6115633659_cdaca906e1_o.jpg")
       (:URL . "http://www.flickr.com/photos/b34u_h4r13y_n47h4n_7h0m45/6115633659/sizes/o/")
       (:MEDIA . "photo"))))
    (:STAT . "ok")))
#|
{ "label": "Square", "width": 75, "height": 75, "source": "http:\/\/farm7.static.flickr.com\/6068\/6115633659_500f8bbd74_s.jpg", "url": "http:\/\/www.flickr.com\/photos\/b34u_h4r13y_n47h4n_7h0m45\/6115633659\/sizes\/sq\/", "media": "photo" }
|#
(defvar *size-square*
  '((:LABEL . "Square") (:WIDTH . 75) (:HEIGHT . 75)
    (:SOURCE . "http://farm7.static.flickr.com/6068/6115633659_500f8bbd74_s.jpg")
    (:URL . "http://www.flickr.com/photos/b34u_h4r13y_n47h4n_7h0m45/6115633659/sizes/sq/")
    (:MEDIA . "photo")))

(defvar *key-tree*
  '((:sizes
     :canblog :canprint :candownload
     (:size
      (:label :width :height :source :url :media)
      (:label :width :height :source :url :media)
      (:label :width :height :source :url :media)
      (:label :width :height :source :url :media)
      (:label :width :height :source :url :media)
      (:label :width :height :source :url :media)
      (:label :width :height :source :url :media)))
    :stat))

(defvar *unique-keys*
    '(:canblog :candownload :canprint :height :label :media :size :sizes :source :stat :url :width))

(defvar *digraph*
    '((:graph
       (:nodes
        #1=((:node-id 1) (:value 1))
        #2=((:node-id 2) (:value 2))
        #3=((:node-id 3) (:value 3))
        #4=((:node-id 4) (:value 4))
        #5=((:node-id 5) (:value 5)))
       (:edges
        ((:edge-id 1) (:src #1#) (:dst #2#))
        ((:edge-id 2) (:src #1#) (:dst #3#))
        ((:edge-id 3) (:src #1#) (:dst #4#))
        ((:edge-id 4) (:src #2#) (:dst #3#))
        ((:edge-id 5) (:src #2#) (:dst #4#))
        ((:edge-id 6) (:src #3#) (:dst #4#))
        ((:edge-id 7) (:src #4#) (:dst #5#))
        ((:edge-id 8) (:src #5#) (:dst #1#))))))

(defvar *circular-list*
    '(#1=(:list . #1#)))

(nst:def-fixtures json-subtree-fixture
    (:documentation "A small sub-tree from a parsed JSON object in list form")
  (json-subtree *default-colors-json*)
  (json-subtree-location (make-location json-subtree)))

(nst:def-fixtures json-tree-fixture
    (:documentation "A full tree from a parsed JSON object in list form")
  (json-tree *flickr-json*)
  (key-tree *key-tree*)
  (unique-keys *unique-keys*)
  (size-square-tree *size-square*)
  (json-tree-location (make-location json-tree)))

(nst:def-fixtures json-nested-list-fixture
    (:documentation "A small subtree from a parsed JSON object with nested lists")
  (json-nested-subtree *page-links*)
  (json-nested-subtree-children
   (zipper-enumeration::%children% (make-location json-nested-subtree)
                                   :visited-tree-nodes (make-hash-table))))

(nst:def-fixtures digraph-fixture
    (:documentation "A directed graph with cycles")
  (digraph *digraph*)
  (digraph-location (make-location digraph)))

(nst:def-fixtures circular-list-fixture
    (:documentation "A cyclic list")
  (circular-list *circular-list*)
  (circular-list-location (make-location circular-list)))

(nst:def-fixtures filtered-values-fixture
    (:documentation "filtered VALUES children from JSON object tree")
  (filtered-values (funcall (key-eq :COLORS-ARRAY) (make-location *default-colors-json*)))
  (filtered-values-children (children filtered-values)))

;;; TEST DEFINITIONS

(nst:def-test-group test-zipper-enumeration (tree-fixture json-subtree-fixture)
  (:documentation "Tests zipper enumeration related functions")
  (:include-groups test-keys test-cycle-handling))

(nst:def-test-group test-keys (json-tree-fixture)
  (:documentation "Test fetching of KEYS from association lists nested in lists")
  (nst:def-test test-keys-lists-only-1
      ;; test well-formed association lists
      (:equal '(a b))
    (keys '((a . 1) (b . 2))))
  (nst:def-test test-keys-lists-only-2
      ;; non-cons values in arbitrary lists
      (:equal '(a b c))
    (keys '((a . 1) (b . 2) 1 nil (3) (4 5 6) (c 7 8 9))))
  (nst:def-test test-fixture-keys
      ;; test if the correct fixture is returned by the zipper
      (:equal size-square-tree)
    (-> json-tree-location #'down #'down #'right #'right #'right #'right
        #'down #'right #'subtree))
  (nst:def-test test-keys-full
      ;; test well-formed association list in JSON-object
      (:equal '(:label :width :height :source :url :media))
    (-> json-tree-location #'down #'down #'right #'right #'right #'right
        #'down #'right #'subtree #'keys))
  (nst:def-test test-all-keys
      (:equal key-tree)
    (all-keys json-tree-location))
  (nst:def-test test-all-unique-keys
      (:equal unique-keys)
    (sort (all-unique-keys json-tree-location) #'string<)))

;; pointer at branch (:bar :baz (:bork))
;; '(:foo (:bar :baz (:bork)))
;;        |
(nst:def-test (test-children :group test-zipper-enumeration)
    (:equal '(:bar :baz (:bork)))
  (map 'list
       (lambda (x) (car x))
       (children (-> (make-location test-tree) #'down #'right))))

(nst:def-test (test-num-children :group test-zipper-enumeration)
    :true (= 8
             (length (children (-> json-subtree-location #'down #'right)))))

(nst:def-test-group test-cycle-handling (digraph-fixture circular-list-fixture)
  (:documentation "Tests if cycles are handled correctly")
  (nst:def-test test-all-unique-keys-cd
      (:equal '(:dst :edge-id :edges :graph :node-id :nodes :src :value))
    (sort (all-unique-keys digraph-location) #'string<))
  (nst:def-test test-all-unique-keys-cl
      (:equal '(:list))
    (sort (all-unique-keys circular-list-location) #'string<))
  (nst:def-test test-all-keys-cl
      (:equal '(:list))
    (all-keys circular-list-location))
  (nst:def-test test-right-locs
      :true (let ((locs (right-locs (-> circular-list-location #'down #'down)
                                    (make-hash-table))))
              (and (= (length locs) 2)
                   (equal (zipper::path-node-left-path-nodes (cadar locs))
                          NIL)
                   (equal (zipper::path-node-left-path-nodes (cadadr locs))
                          '(:list)))))
  (nst:def-test test-left-locs
      :true (let ((locs (left-locs (-> circular-list-location
                                     #'down #'down #'right #'right #'right #'right)
                                   (make-hash-table))))
              (and (= (length locs) 3)
                   (equal (zipper::path-node-left-path-nodes (cadar locs))
                          '(:list :list :list :list))
                   (equal (second locs) :list)
                   (equal (zipper::path-node-left-path-nodes (third locs))
                          '(:list :list :list)))))
  (nst:def-test test-threading-with-filter
      (:equal '(:list))
    (zip-> circular-list-location :list #'subtree)))


;; FILTER PREDICATES

(nst:def-test-group test-filter-predicates (json-subtree-fixture)
  (:documentation "Tests JSON object-tree specific filter predicates")
  (nst:def-test test-%key-eq%
      :true (zipper-enumeration::%key-eq% (down json-subtree-location) :NAME))
  (nst:def-test test-key-eq-1
      (:equal "Default Colors")
    (cdaar (funcall (key-eq :NAME) json-subtree-location)))
  (nst:def-test test-key-eq-2
      (:eq 7)
    (length (funcall (key-eq :COLORS-ARRAY) json-subtree-location))))

(nst:def-test-group test-filterpredicates-and-access (json-subtree-fixture)
  (:documentation
   "Tests JSON specific filter predicates and value access functions")
  (:include-groups test-filter-predicates)
  (nst:def-test test-subtree-key-trivial
      (:eq 'PI-VALUE)
    (subtree-key (make-location '(PI-VALUE . "3.141593"))))
  (nst:def-test test-subtree-value-trivial
      (:equal "3.141593")
    (subtree-value (make-location '(PI-VALUE . "3.141593"))))
  (nst:def-test test-num-value-trivial
      (:eql 3.141593)
    (first (multiple-value-list (num-value (make-location '(PI-VALUE . "3.141593"))))))
  (nst:def-test test-num-value-negative-1
      :true
    (null (num-value (make-location '(TIME . "12:20")))))
  (nst:def-test test-num-value-negative-2
      :true
    (null (num-value (make-location '(QUESTION . "#.(format t \"What\")")))))
  (nst:def-test test-num-value-negative-3
      :true
    (null (num-value (make-location '(NOUN . "algorithm")))))
  (nst:def-test test-num-value-negative-4
      :true
    (null (num-value (make-location '(SYMBOL . :lambda)))))
  (nst:def-test test-num-value-negative-5
      :true
    (null (num-value '(1000))))
  (nst:def-test num-value-equal
      :true
    (zipper-enumeration::%num-value-equal%
     (make-location '(PI-VALUE . "3.141593"))
     3.141593))
  (nst:def-test num-value-equal-negative
      :true
    (null (zipper-enumeration::%num-value-equal%
            (make-location '(PI-VALUE . "PI"))
            3.141593)))
  (nst:def-test test-subtree-keys
      (:equal '(:NAME :COLORS-ARRAY))
    (subtree-keys json-subtree-location))
  (nst:def-test test-subtree-key
      (:eq :NAME)
    (subtree-key (first (funcall (key-eq :NAME) json-subtree-location))))
  (nst:def-test test-subtree-value
      (:equal "Default Colors")
    (subtree-value (first (funcall (key-eq :NAME) json-subtree-location))))
  (nst:def-test test-text-value-1
      (:equal "Default Colors")
    (text-value (first (funcall (key-eq :NAME) json-subtree-location))))
  (nst:def-test test-text-value-2
      (:equal "42")
    (text-value (make-location '(NUMBER . 42))))
  (nst:def-test test-text-value-equal
      :true
    (zipper-enumeration::%text-value-equal%
     (make-location '(NAME . "Default Colors"))
     "Default Colors"))
  (nst:def-test test-text-value-equal-negative-1
      :true
    (null (zipper-enumeration::%text-value-equal%
            (make-location '(NAME . "fault Colors"))
            "Default Colors")))
  (nst:def-test test-text-value-equal-negative-2
      :true
    (null (zipper-enumeration::%text-value-equal%
            (make-location '(SYMBOL . :foo))
            "Default Colors")))
  (nst:def-test test-subtree-value-text
      (:eq :NAME)
    (subtree-key (first (funcall (text-value-equal "Default Colors") json-subtree-location))))
  (nst:def-test test-num-value
      (:equal "#f00")
    (let ((loc (second (children (-> json-subtree-location #'down #'right)))))
      (text-value (first (funcall (key-eq :HEX-VALUE) loc))))))

(nst:def-test-group test-filterpredicates-nested-list (json-nested-list-fixture)
  (:documentation
   "Tests JSON specific filter predicates using JSON structures with nested lists")
  (:include-groups test-filterpredicates-and-access)
  (nst:def-test test-%children%
      (:equal json-nested-subtree)
    (mapcar #'zipper:subtree
            (zipper-enumeration::%children% (make-location json-nested-subtree)
                                            :visited-tree-nodes (make-hash-table))))
  (nst:def-test test-key-eq-1
      (:equal '((:ID . "1") (:ID . "2")))
    (mapcar #'subtree (funcall (key-eq :ID) json-nested-subtree-children)))
  (nst:def-test test-key-eq-2
      (:equal NIL)
    (funcall (key-eq :ID) (make-location NIL)))
  (nst:def-test test-subtree-value-text-1
      (:eq :URL)
    (subtree-key (first (funcall (text-value-equal "http://nodejs.org")
                                 (make-location json-nested-subtree)))))
  (nst:def-test test-subtree-value-number-1
      (:eq :ID)
    (subtree-key (first (funcall (num-value-equal 2)
                                 (make-location json-nested-subtree)))))
  (nst:def-test test-subtree-text-1
      (:equal "green")
    (subtree-text (make-location "green")))
  (nst:def-test test-subtree-text-2
      (:equal "255")
    (subtree-text (make-location 255))))

(nst:def-test-group test-chained-filters (filtered-values-fixture)
  (:documentation "Tests chaining of JSON specific filter predicates")
  (:include-groups test-filterpredicates-and-access)
  (nst:def-test test-first-color-value-1
      (:eql 1)
    (length (funcall (key-eq :COLOR-NAME) (first filtered-values-children))))
  (nst:def-test test-first-color-value-2
      (:equal "red")
    (subtree-value (first (funcall (key-eq :COLOR-NAME) (first filtered-values-children)))))
  (nst:def-test test-color-values-1
      (:eql 7)
    (length (map 'list (key-eq 'zipper::COLOR-NAME) filtered-values)))
  (nst:def-test test-color-values-2
      (:equal '(:COLOR-NAME :COLOR-NAME :COLOR-NAME :COLOR-NAME :COLOR-NAME :COLOR-NAME :COLOR-NAME))
    (mapcar (lambda (x) (subtree-key (car x)))
            (mapcar (key-eq :COLOR-NAME)
                    filtered-values))))

(nst:def-test-group test-json-filter (json-subtree-fixture json-tree-fixture)
  (:documentation "Tests filtering using zippers")
  (nst:def-test test-apply-filter-sequentially
      (:equal '("#f00" "#0f0" "#00f" "#0ff" "#f0f" "#ff0" "#000"))
    (zip-> json-subtree-location :COLORS-ARRAY :HEX-VALUE #'subtree-value))
  (nst:def-test test-threading-with-filters
    (:equal '(:label :width :height :source :url :media))
    (reduce #'union (zip-> json-tree-location :size #'subtree-keys)))
  (nst:def-test test-threading-text-value-filter
      (:equal '(:label))
    (zip-> json-tree-location "Medium 640" #'subtree-key))
  (nst:def-test test-threading-num-value-filter
      (:equal '(:width))
    (zip-> json-tree-location 1440 #'subtree-key)))
