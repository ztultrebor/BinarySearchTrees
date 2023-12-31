;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname binarysearchtrees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

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


(define (add-to-tree node tree)
  ; BST BST -> BST
  ; adds a new node into a tree and rebalances it
  (rebalance (insert-node node tree)))


(define (insert-node node tree)
  ; BST BST -> BST
  ; inserts a new node into a tree in the correct leaf,
  ; but without rebalancing the tree
  (cond
    [(false? tree) node]
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
  (cond
    [(false? tree) #f]
    [(> (- (count-nodes (bst-lbranch tree))
           (count-nodes (bst-rbranch tree))) 1)
     (rebalance (purge-duplicates
                 (insert-node
                  (make-bst (bst-key tree) (bst-val tree)
                            #f (bst-rbranch tree))
                  (insert-node (bst-lbranch tree)
                               (rightmost (bst-lbranch tree))))))]
    [(> (- (count-nodes (bst-rbranch tree))
           (count-nodes (bst-lbranch tree))) 1)
     (rebalance (purge-duplicates
                 (insert-node
                  (make-bst (bst-key tree) (bst-val tree)
                            (bst-lbranch tree) #f)
                  (insert-node (bst-rbranch tree)
                               (leftmost (bst-rbranch tree))))))]
    [else (make-bst (bst-key tree) (bst-val tree)
                    (rebalance (bst-lbranch tree))
                    (rebalance (bst-rbranch tree)))]))


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


(define (purge-duplicates tree)
  ; BST BST -> BST
  ; eliminate any duplicates of the root node of tree
  (make-bst (bst-key tree) (bst-val tree)
            (purge (bst-key tree) (bst-lbranch tree))
            (purge (bst-key tree) (bst-rbranch tree))))


(define (purge key tree)
  ; Natural BST -> BST
  ; delete node with given key from tree
  (cond
    [(false? tree) #f]
    [(= (bst-key tree) key)
     (cond
       [(false? (bst-lbranch tree)) (bst-rbranch tree)]
       [(false? (bst-rbranch tree)) (bst-lbranch tree)]
       [else tree])]
    [(< key (bst-key tree))
     (make-bst (bst-key tree) (bst-val tree)
               (purge key (bst-lbranch tree)) (bst-rbranch tree))]
    [(> key (bst-key tree))
     (make-bst (bst-key tree) (bst-val tree)
               (bst-lbranch tree) (purge key (bst-rbranch tree)))]))

  
; ===========================
; checks
(define BST1 (make-bst 1 "m" false false))
(define BST2 (make-bst 2 "x" false false))
(define BST3 (make-bst 3 "w" false false))
(define BST4 (make-bst 4 "k" false false))
(define BST5 (make-bst 5 "g" false false))
(define BST7 (make-bst 7 "p" false false))
(define twee (insert-node
              BST7
              (insert-node
               BST5
               (insert-node
                BST4
                (insert-node BST3 (insert-node BST2 BST1))))))
(check-expect (lookup 2 twee) "x")
(check-expect (lookup 6 twee) #f)
(check-expect (insert-node BST1 #f) BST1)
(check-expect (insert-node BST4 BST1) (make-bst 1 "m" false BST4))
(check-expect (insert-node BST7 (insert-node BST4 BST1))
              (make-bst 1 "m" false (make-bst 4 "k" false BST7)))
(check-expect (rebalance (insert-node BST7 (insert-node BST4 BST1)))
              (make-bst 4 "k" (make-bst 1 "m"  #f #f) (make-bst 7 "p" #f #f)))
(check-expect (add-to-tree
                BST4
                (rebalance (insert-node
                            BST7
                            (insert-node
                             BST5
                             (insert-node BST3 (insert-node BST1 BST2))))))
              (rebalance twee))
(check-expect (count-nodes (insert-node BST7 (insert-node BST4 BST1))) 3)
(check-expect (purge 1 BST1) #f)
(check-expect (purge 1 (insert-node BST1 BST3)) BST3)
(check-expect (purge 1 (insert-node BST3 BST1)) BST3)
(check-expect (purge 1
                     (insert-node (insert-node BST4 (insert-node BST1 BST3))
                                  (insert-node BST7 BST5)))
              (insert-node BST7 (insert-node BST4
                                             (insert-node BST3 BST5))))
(check-expect (inorder twee) '("m" "x" "w" "k" "g" "p"))