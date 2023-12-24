;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname binarysearchtrees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; data definitions

(define-struct bst [key val lbranch rbranch])
; A BinarySearchTree (BST) is a [Number Any Branch Branch]
; A Branch is one of:
;    - false
;    - BinarySearchTree
; Every key in the left branch is less than the key of the parent
; Every key in the right branch is greater than the key of the parent
#;
(define (fn-on-bst bst)
  (cond
    [(false? bst) ...]
    [else (cond
            [(fn-on-number bst-key) (fn-on-any bst-val)]
            [(fn-on-number bst-key) (fn-on-bst bst-lbranch)]
            [(fn-on-number bst-key) (fn-on-bst bst-rbranch)])]))
                   


;; constants

(define BST1 (make-bst 1 "m" false false))
(define BST4 (make-bst 4 "k" false false))
(define BST7 (make-bst 7 "p" false false))



; functions
(define (add-to-tree branch tree)
  ; BST BST -> BST
  ; add a new branch to an ecisting tree
  (cond
    [(false? tree) branch]
    [else (cond
            [(< (bst-key branch) (bst-key tree))
             (make-bst
              (bst-key tree) (bst-val tree)
              (add-to-tree branch (bst-lbranch tree)) (bst-rbranch tree))]
            [(> (bst-key branch) (bst-key tree))
             (make-bst
              (bst-key tree) (bst-val tree) (bst-lbranch tree)
              (add-to-tree branch (bst-rbranch tree)) )])]))
; checks
(check-expect (add-to-tree BST4 BST1) (make-bst 1 "m" false BST4))
(check-expect (add-to-tree BST7 (add-to-tree BST4 BST1))
              (make-bst 1 "m" false (make-bst 4 "k" false BST7)))


