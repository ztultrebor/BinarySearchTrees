;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname binarysearchtrees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; data definitions

(define-struct bst [key val lbranch rbranch])
; A BinarySearchTree (BST) is a [Natural X Branch Branch]
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
                   

; ==========================
; functions

(define (create-bst kvpair)
  ; Number X -> BST
  ; creates a sungle-node BST with key and val
  (make-bst (first kvpair) (second kvpair) #f #f))


(define (create-bst-from-list kvlist)
  ; [ListOf [Number X]] -> BST
  ; creates a (probably unbalanced) BST from a list of key value pairs
  (foldr insert-node #f (map create-bst kvlist)))


(define (insert-node node tree)
  ; BST BST -> BST
  ; inserts a new node into a tree in the correct leaf,
  ; but without rebalancing the tree
  (cond
    [(false? tree) node]
    [(false? node) tree]
    [else (cond
            [(< (bst-key node) (bst-key tree))
             (make-bst
              (bst-key tree) (bst-val tree)
              (insert-node node (bst-lbranch tree)) (bst-rbranch tree))]
            [(> (bst-key node) (bst-key tree))
             (make-bst
              (bst-key tree) (bst-val tree) (bst-lbranch tree)
              (insert-node node (bst-rbranch tree)))]
            [(= (bst-key node) (bst-key tree)) tree])]))


(define (rebalance tree)
  ; BST -> BST
  ; rebalances the tree when the branches become uneven
  (local (
          (define (purge key tree)
            ; Natural BST -> BST
            ; purge a node with given key,
            ; along with all its subbranches from tree
            (cond
              [(false? tree) #f]
              [(= (bst-key tree) key) #f]
              [(< key (bst-key tree))
               (make-bst (bst-key tree) (bst-val tree)
                         (purge key (bst-lbranch tree)) (bst-rbranch tree))]
              [(> key (bst-key tree))
               (make-bst (bst-key tree) (bst-val tree)
                         (bst-lbranch tree) (purge key (bst-rbranch tree)))]))
          (define (reorganizer new-root old-main pruner)
            ; BST BST BST -> BST
            (rebalance
             (insert-node old-main
                          (insert-node
                           (purge (bst-key new-root) pruner) 
                           new-root)))))
    ; - IN -
    (cond
      [(false? tree) #f]
      [(> (- (count-nodes (bst-rbranch tree))
             (count-nodes (bst-lbranch tree))) 1)
       (reorganizer (leftmost (bst-rbranch tree))
                    (make-bst (bst-key tree) (bst-val tree) (bst-lbranch tree) #f)
                    (bst-rbranch tree))]
      [(> (- (count-nodes (bst-lbranch tree))
             (count-nodes (bst-rbranch tree))) 1)
       (reorganizer (rightmost (bst-lbranch tree))
                    (make-bst (bst-key tree) (bst-val tree) #f (bst-rbranch tree))
                    (bst-lbranch tree))]
      [else (make-bst (bst-key tree) (bst-val tree)
                      (rebalance (bst-lbranch tree))
                      (rebalance (bst-rbranch tree)))])))




     
(define (lookup key tree)
  ; Natural BST -> Any
  ; search a BST for the provided key and return the
  ; corresponding value if it's there; else #false
  (cond
    [(false? tree) #f]
    [(= key (bst-key tree)) (bst-val tree)]
    [(< key (bst-key tree)) (lookup key (bst-lbranch tree))]
    [(> key (bst-key tree)) (lookup key (bst-rbranch tree))]))


(define (inorder tree)
  ; BST -> [ListOf X]
  ; returns a list of values from the BST,
  ; ordered from left to right across the tree
  (cond
    [(false? tree) '()]
    [else (append (inorder (bst-lbranch tree))
                  (list (bst-val tree))
                  (inorder (bst-rbranch tree)))]))


(define (count-nodes tree)
  ; BST -> Natural
  ; counts up the number of nodes in a tree
  (cond
    [(false? tree) 0]
    [else (+ (count-nodes (bst-lbranch tree))
             (count-nodes (bst-rbranch tree)) 1)]))


(define (rightmost tree)
  ; BST -> BST
  ; follow the tree to its right-mostest leaf
  (cond
    [(false? (bst-rbranch tree)) tree]
    [else (rightmost (bst-rbranch tree))]))


(define (leftmost tree)
  ; BST -> BST
  ; follow the tree to its left-mostest leaf
  (cond
    [(false? (bst-lbranch tree)) tree]
    [else (leftmost (bst-lbranch tree))]))


; ===========================
; checks

(define kvlist (create-bst-from-list '((25 x) (13 x) (20 x) (10 x) (7 x)
                                              (1 x) (15 x) (4 x) (9 x))))
(define dogma (create-bst-from-list '((99 o) (77 l) (24 i) (10 h) (95 g)
                                              (15 d) (89 c) (29 b) (63 a))))
(check-expect (create-bst '(5 x)) (make-bst 5 'x #f #f))
(check-expect (create-bst-from-list '((5 m))) (make-bst 5 'm #f #f))
(define babi-list-pt1 (create-bst-from-list '((1 x) (2 x))))
(define babi-list-pt2 (create-bst-from-list '((5 x) (4 x) (3 x))))
(define merged-babi (create-bst-from-list '((5 x) (4 x) (3 x) (1 x) (2 x))))
(define searchy-babi (create-bst-from-list '((1 x) (5 x) (4 x) (2 x) (3 x))))
(check-expect (insert-node babi-list-pt2 babi-list-pt1) merged-babi)
(check-expect (rebalance babi-list-pt2) (create-bst-from-list
                                         '((5 x) (3 x) (4 x))))
(check-expect (rebalance searchy-babi) searchy-babi)
(check-expect (rebalance merged-babi) searchy-babi)



(rebalance kvlist)

(rebalance dogma)

dogma