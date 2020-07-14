;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: ; Base: 10 -*-
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        json-zip.lisp
;;; LANGUAGE:    Common-Lisp
;;;
;;; DESCRIPTION
;;;  Filtering of tree-like datastructures
;;;
;;;  threading, zipper, zipper-enumeration,
;;;  datastructure (JSON) specific filter-predicates
;;;
;;;
;;; Author: Christian Hofmann-Fuchs
;;;
;;; Created: So Aug 30 22:05:27 2015 (+0200)
;;;
;;; Last-Updated: Mo Jul 13 21:18:32 2020 (+0200)
;;;           By: Christian Hofmann-Fuchs
;;;           Update #: 1354
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
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;
;;;****************************************************************************

(in-package :cl-user)
(defpackage #:zipper
  (:use :cl)
  (:export #:make-location
           #:path
           #:subtree
           #:down
           #:left
           #:right
           #:left-locs
           #:right-locs
           #:branch-p
           #:leftmost-p
           #:rightmost-p
           #:print-object
           ))
(in-package #:zipper)

;; location = tree + path
;; path = zipper

;;--------
;; zipper
;;--------

(defstruct path-node
  "A node of the zipper"
  (left-path-nodes)
  (right-path-nodes)
  (path))

;; the zipper
(deftype path () '(or nil path-node))

(defmethod print-object ((self path-node) stream)
  (handler-case
      (macrolet ((length-or-object (structure accessor)
                   `(if (listp (,accessor ,structure))
                        (length (,accessor ,structure))
                      (,accessor ,structure))))
        (format stream "#LOC{~A:~A:~A}"
                (length-or-object self path-node-left-path-nodes)
                (length-or-object self path-node-path)
                (length-or-object self path-node-right-path-nodes)))
    (error (condition)
      (warn (write-to-string condition))
      (call-next-method self stream))))

(defun make-location (lst)
  "RETURNs a new location that represents the current location in the tree.
A location consists of a subtree with subtree-nodes and leaf-nodes
represented by a (nested) list LST and a path.
Initially the path is NIL."
  (list lst nil))

;;----------------
;; zipper movement
;;----------------

;; not implemented: next,previous, up

(defun down (loc)
  "RETURNs the location of the leftmost element of the subtree at location LOC,
or NIL if the location is at a leaf-node."
  (let ((subtree (car loc))
        (path (cadr loc)))
    (when (listp subtree)
      (list (car subtree)
            (make-path-node :left-path-nodes nil
                            :right-path-nodes (cdr subtree)
                            :path path)))))

(defun right (loc)
  "RETURNs the location of the first right neighbour of the node
at location LOC, or NIL if no right neighbour exists."
  (let ((subtree (car loc))
        (path (cadr loc)))
    (when path
      (let ((left (path-node-left-path-nodes path))
            (right (path-node-right-path-nodes path)))
        (when right
          (list (car right)
                (make-path-node :left-path-nodes (cons subtree left)
                                :right-path-nodes (cdr right)
                                :path (path-node-path path))))))))

(defun left (loc)
  "RETURNs the location of the first left neighbour of the node
at location LOC, or NIL if no left neighbour exists."
  (let ((subtree (car loc))
        (path (cadr loc)))
    (when path
      (let ((left (path-node-left-path-nodes path))
            (right (path-node-right-path-nodes path)))
        (when left
          (list (car left)
                (make-path-node :left-path-nodes (cdr left)
                                :right-path-nodes (cons subtree right)
                                :path (path-node-path path))))))))

;;--------------
;; zipper access
;;--------------

;; not implemented: path

(declaim (inline subtree))
(defun subtree (loc)
  "RETURNs the subtree at location LOC. The subtree is either a list
representing a subtree-node or an atom representing a leaf-node."
  (car loc))

(declaim (inline branch-p))
(defun branch-p (loc)
  "RETURNS T if location LOC is at a branch in the tree,
i.e. it points to a subtree-node represented by a list"
  (let ((subtree (car loc)))
    (and (listp subtree)
         (listp (cdr subtree)))))

;;----------------
;; zipper location
;;----------------

(defun right-locs (loc visited-tree-nodes)
  "RETURNS all locations to the right of the location LOC, including LOC"
  (when loc
    (cond
     ((gethash (car loc) visited-tree-nodes)
      (list loc))
     (T
      ;; add the tree-node at location to visited tree-nodes
      (setf (gethash (car loc) visited-tree-nodes) T)
      (cons loc (right-locs (right loc) visited-tree-nodes))))))

(defun left-locs (loc visited-tree-nodes)
  "RETURNS all locations to the left of loc, starting with LOC"
  (when loc
    (cond
     ((gethash (car loc) visited-tree-nodes)
      loc)
     (T
      ;; add the tree-node at location to visited tree-nodes
      (setf (gethash (car loc) visited-tree-nodes) T)
      (cons loc (left-locs (left loc) visited-tree-nodes))))))

(defun leftmost-p (loc)
  "RETURN T if there are no more tree-nodes to the left of location LOC"
  (null (left loc)))

(defun rightmost-p (loc)
  "RETURN T if there are no more tree-nodes to the right of location LOC"
  (null (right loc)))

;;---------------
;; zipper editing
;;---------------

;; not implemented: edit, insert, append, remove, replace


;;;=================
;;; Threading
;;;=================

(in-package :cl-user)
(defpackage #:threading
  (:use :cl)
  (:export #:->))
(in-package #:threading)

;;-----------
;; Threading
;;-----------

;; apply-sequentially
(defmacro -> (value &body functions)
  "Sequentially apply functions from list FUNCTIONS to the result from
the previously applied function, starting with an initial form VALUE.
a.k.a 'Threading' (Clojure)"
  `(reduce (lambda (last-result fun) (funcall fun last-result))
           (list ,@functions)
           :initial-value ,value))

;;;===================
;;; Zipper Enumeration
;;;===================

(in-package :cl-user)
(defpackage #:zipper-enumeration
  (:use :cl :threading :zipper)
  (:export #:keys
           #:all-keys
           #:all-unique-keys
           #:children
           #:zip->
           #:location-p
           #:key-eq
           #:subtree-key
           #:subtree-value
           #:subtree-text
           #:num-value
           #:num-value-equal
           #:text-value
           #:text-value-equal
           #:subtree-keys
          ))
(in-package #:zipper-enumeration)

;;; JSON specific enumeration
;;;  - tree, lists of leaf-nodes, key-value pair leaf-nodes repr. by conses

;; Example: (-> (make-location *tree*) #'down #'down #'right #'subtree #'keys)
(defun keys (list)
  "RETURNs the keys from a list, where keys can be keys of an association list or
atoms at the start of a list"
  (if (listp list)
      (loop :for elt :in list
            :when (and (consp elt) (symbolp (car elt)))
              :collect (car elt))
      ()))

(defun children (loc &key (visited-tree-nodes (make-hash-table)))
  "RETURNs all immediate childrens of the tree-node at location LOC,
or NIL if the tree-node is a leaf-node."
  (when (branch-p loc)
    (map 'list
         ;; add subtree at focus to visited-tree-nodes
         #'(lambda (x) (setf (gethash (car x) visited-tree-nodes) T) x)
         (right-locs (down loc) visited-tree-nodes))))

(defun all-keys (loc &key (visited-tree-nodes (make-hash-table)))
  "RETURNs a subtree with all keys from a location, where keys can be
keys of an association list or atoms at the start of a list"
  (let ((keys ())
        (result ())
        (subtree (car loc)))
    (unless (gethash subtree visited-tree-nodes)
      (setf (gethash subtree visited-tree-nodes) T)
      (dolist (elt (children loc) (nreverse keys))
        (setq result
              (if (branch-p elt)
                  (if (atom (caar elt))
                      ;; element at loc is a key
                      (cons (caar elt) (all-keys elt :visited-tree-nodes visited-tree-nodes))
                      ;; element at loc is a list of keys
                      (all-keys elt :visited-tree-nodes visited-tree-nodes))
                  ;; get all keys of the leaf element
                  (reduce #'append (keys elt))))
        (cond
          ((and (atom result) (not (null result)))
           (push result keys))
          ((= (length result) 1)
           (push (car result) keys))
          ((> (length result) 1)
           (push result keys)))))))

(defun all-unique-keys (loc &key (visited-tree-nodes (make-hash-table)))
  "RETURNs a list with all unique keys from a zipper, where keys can be
keys of an association list or atoms at the start of a list.
The keys may appear in the result list in any arbitrary order."
  (let ((subtree (car loc)))
    (unless (gethash subtree visited-tree-nodes)
      (setf (gethash subtree visited-tree-nodes) T)
      (union (keys loc)
             (reduce #'union
                     (loop :for elt :in (children loc)
                           :collect (if (branch-p elt)
                                        (all-unique-keys elt :visited-tree-nodes visited-tree-nodes)
                                        (keys elt))))))))

;;;-----------------
;;; filter threading
;;;-----------------

;; extensions to simple zipper enumeration by introducing the concept of filter predicates,
;; and by enumerating lists of elements returned by a filter instead of lists of locations

(defun %children% (loc &key visited-tree-nodes) ;
  "RETURNs all immediate childrens of location LOC, and remove the subtree-nodes they are pointing at from the list of visited-tree-nodes, such that they can be descended the next time"
  (when (branch-p loc)
    (map 'list
         #'(lambda (x) (remhash (car x) visited-tree-nodes) x) ; remove subtree-node from visited tree-nodes
         (right-locs (down loc) visited-tree-nodes))))

(defun %apply-predicate% (predicate loc &key visited-tree-nodes)
  (let ((value (funcall predicate loc :visited-tree-nodes visited-tree-nodes)))
    ;(format t "~%~A" value)
    (cond
      ;; if predicate returns true return the current location
      ((eq value T) (list loc))
      ;; if predicate returns nil stop
      ((null value) nil)
      ;; if predicate returns a list return it
      ((listp value) value)
      ;; in all other cases assume its a location
      (t (list value)))))

(defun %make-predicate% (predicate-element)
  "PREDICATE-ELEMENT can be one of symbol (i.e. key),
string/number (i.e. value), function (i.e. predicate function),
or sequence (i.e. subquery)"
  (etypecase predicate-element
    ;; key
    (symbol (key-eq predicate-element))
    ;; string-value
    (string (text-value-equal predicate-element))
    ;; numeric-value
    (number (num-value-equal predicate-element))
    ;; subquery
    (sequence
     (lambda (loc &key visited-tree-nodes)
       (declare (ignore visited-tree-nodes))
       (and (list (zip-> loc predicate-element)) (list loc))))
    ;; predicate
    (function
     (lambda (loc &key visited-tree-nodes)
      (declare (ignore visited-tree-nodes))
      (list (funcall (the function predicate-element) loc))))))

;; zipper-enumeration-filter

;; applies to the current location, or when matched previously, to the children of the current location that are marked as matched
;; goal: stop when the first match is found, and support nested keys
;; e.g. (:a . (:a . 0)) (zip-> loc :a :a) => (:a . 0), (zip-> loc :a) => (:a . (:a . 0))

;; apply-filter-sequentially
(defun zip-> (loc &rest predicates)
  "Sequentially apply filters from list PREDICATES to the result from
the previously applied filter function, starting with the location LOC.
PREDICATES is a list with  symbols (i.e. key), or functions
(i.e. predicate function)."
  (let ((visited-tree-nodes (make-hash-table :size 1023)))
    (flet ((%filter-locations% (loc-list predicate-element)
             (loop :for loc :in loc-list
                   :append (%apply-predicate% (%make-predicate% predicate-element)
                                  loc
                                  :visited-tree-nodes visited-tree-nodes))))
      (reduce
       #'%filter-locations%
       predicates
       :initial-value (list loc)))))

;; would like something along the lines of (defgeneric filter-predicate (self loc &key visited-tree-nodes))
;; more exactly, function types would be great here
;; simple solution: (defmacro deftfun (name args &key ftype  &rest body)
;;  (unless (equalp args (cdr (assoc ftype *ftype-arg-list*)))
;;          (error "Wrong argument list for type ~A" ftype)))
;;

;;; -------------------------------
;;; JSON specific FILTER PREDICATES
;;; -------------------------------

;;; zipper-enumeration-filter-predicates-json

;;; filter predicates have to conform to take the form (lambda (zipper :visited-tree-nodes visited-tree-nodes) ...)
;;; they should return a list of zippers pointing to the elements that satisfy the predicate

(defun location-p (element)
  "RETURNS T if element ELEMENT is a location"
  (and (listp element)
       (typep (second element) 'zipper::path-node)))

;; (%key-eq% (down (make-location *default-colors-json*)) 'NAME)
(defun %key-eq% (loc key)
  "test car of location LOC"
  (when (consp (subtree loc))
    (eq key (car (subtree loc)))))

(defmacro def-filter (predicate value)
  "DEFINEs a filter function for predicate PREDICATE that matches the value VALUE.
RETURNs a function value"
  `(labels ((%collect-values% (loc &key (visited-tree-nodes (make-hash-table)))
              "recursively visit all children of the tree-node at location LOC.
               If a subtree-node with a key equal to VALUE is found,
               return all children of this tree-node.
               Use hash-table VISITED-TREE-NODES to detect cycles."
              (loop :for elt
                    :in (cond
                          ((not (gethash (car loc) visited-tree-nodes))
                           (setf (gethash (car loc) visited-tree-nodes) T)
                           (list loc))
                          (T
                           (%children% loc :visited-tree-nodes visited-tree-nodes)))
                    :append  (if (,predicate elt ,value)
                                 (let ((child-locations (%children% elt :visited-tree-nodes visited-tree-nodes)))
                                   ;(format t "~%CDR Child-Locations: ~A ~A" child-locations (cdr child-locations))
                                   (if child-locations
                                       (cdr child-locations)
                                       (list elt)))
                                 (when (car elt)
                                   (%collect-values% elt :visited-tree-nodes visited-tree-nodes))))))
     (function %collect-values%)))

(defun key-eq (key)
  "RETURNs a filter function for key KEY.
The filter function takes a location as argument and returns a list of zippers, that point to elements with key equal to KEY."
  (def-filter %key-eq% key))

;;;--------------------------
;;; JSON specific ENUMERATION
;;;--------------------------

;; (subtree-keys (make-location *test-2*)) -> (id name values)
(defun subtree-keys (loc)
  "RETURNs the keys from a list at zipper location LOC, where keys can be keys of an association list, or atoms at the start of a list"
  (let ((list (subtree loc)))
   (keys list)))

;;;------------------------
;;; JSON specific ACCESSORS
;;;------------------------

(defmacro when-pair ((var loc) form)
  "EXECUTE form FORM if the zipper location LOC points to a cons-cell."
  `(let ((,var (subtree ,loc)))
     (when (and (consp ,var) (not (listp (cdr ,var))))
         ,form)))

;; (subtree-text (make-location "a")) -> "a"
(defun subtree-text (loc)
  "RETURNs the VALUE of the subtree-node pointed at by the location LOC as string"
  (let ((value (subtree loc)))
    (if (stringp value)
        value
        (write-to-string value))))

;; (subtree-value (make-location '(WIDTH . "240"))) -> "240"
(defun subtree-value (loc)
  "RETURNs the VALUE of the key-value pair subtree-node pointed at by the location LOC"
  (when-pair (pair loc)
    (cdr pair)))

;; (subtree-key (make-location '(WIDTH . "240"))) -> WIDTH
(defun subtree-key (loc)
  "RETURNs the KEY of the key-value pair subtree-node pointed at by the location LOC"
  (when-pair (pair loc)
    (car pair)))

;; (num-value (make-location '(WIDTH . "240"))) -> 240
(defun num-value (loc)
  "RETURNs the value converted to a NUMBER of the key-value pair subtree-node pointed at by the location LOC, or NIL if the value can't be interpreted as number."
  (let ((value (subtree-value loc)))
    (cond ((numberp value)
           value)
          ((stringp value)
           (let ((*read-base* 10)
                 (*read-default-float-format* 'single-float)
                 (*read-eval* nil))
             (ignore-errors
               (let ((result (read-from-string (the string value))))
                 (if (numberp result)
                     result
                     NIL)))))
          (t NIL))))

;; (%num-value-equal% (make-location '(WIDTH . "240")) 240)
(defun %num-value-equal% (loc number)
  "RETURNs T if the value of the subtree-node at the location LOC
is equal to the number NUMBER."
  (let ((value (num-value loc)))
    (equal value number)))

(defun num-value-equal (number)
    "RETURNs a filter function for number NUMBER.
The filter function takes a location as argument and returns a list of zippers, that point to elements with value equal to NUMBER."
    (def-filter %num-value-equal% number))

;; (text-value '(LABEL . "Square"))
(defun text-value (loc)
  "RETURN the value at zipper location LOC as string.
NIL if there is no key-value pair at LOC"
  (when-pair (pair loc)
    (let ((value (cdr pair)))
      (if (stringp value)
          value
          (write-to-string value)))))

;; (%text-value-equal% (make-location '(LABEL . "Square")) "Square")
(defun %text-value-equal% (loc text)
  "RETURNs T if the value of the subtree-node at the location LOC
is equal to the string TEXT."
  (let ((value (subtree-value loc)))
    (and (stringp value) (equal value text))))

(defun text-value-equal (text)
  "RETURNs a filter function for string TEXT.
The filter function takes a location as argument and returns a list of locations, that point to elements with value equal to TEXT."
  (def-filter %text-value-equal% text))
