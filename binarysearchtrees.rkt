;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname binarysearchtrees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; data definitions

(define-struct bst [key val lbranch rbranch])
; A BinarySearchTree (BST) is a [Natural Any Branch Branch]
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
(define BST3 (make-bst 3 "w" false false))
(define BST4 (make-bst 4 "k" false false))
(define BST5 (make-bst 5 "g" false false))
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
              (add-to-tree branch (bst-rbranch tree)))]
            [(= (bst-key branch) (bst-key tree)) tree])]))
; checks
(check-expect (add-to-tree BST4 BST1) (make-bst 1 "m" false BST4))
(check-expect (add-to-tree BST7 (add-to-tree BST4 BST1))
              (make-bst 1 "m" false (make-bst 4 "k" false BST7)))


(define (rebalance tree)
  ; BST -> BST
  ; rebalances the tree when the branches become uneven
  (cond
    [(> (- (count-nodes (bst-lbranch tree))
           (count-nodes (bst-rbranch tree))) 1)
     (purge-duplicates (add-to-tree (make-bst (bst-key tree) (bst-val tree)
                                              #f (bst-rbranch tree))
                                    (add-to-tree (bst-lbranch tree)
                                                 (rightmost (bst-lbranch tree)))))]
    [(> (- (count-nodes (bst-rbranch tree))
           (count-nodes (bst-lbranch tree))) 1)
     (purge-duplicates (add-to-tree (make-bst (bst-key tree) (bst-val tree)
                                              (bst-lbranch tree) #f)
                                    (add-to-tree (bst-rbranch tree)
                                                 (leftmost (bst-rbranch tree)))))]
    [else tree]))
(check-expect (rebalance (add-to-tree BST7 (add-to-tree BST4 BST1)))
              (make-bst 4 "k" (make-bst 1 "m"  #f #f) (make-bst 7 "p" #f #f)))
(check-expect (rebalance
               (add-to-tree (add-to-tree BST4 (add-to-tree BST1 BST3))
                            (add-to-tree BST7 BST5)))
              (add-to-tree BST1
                           (add-to-tree BST7
                                        (add-to-tree BST3
                                                     (add-to-tree BST5 BST4)))))


(define (count-nodes tree)
  ; BST -> Natural
  ; counts up the number of nodes in a tree
  (cond
    [(false? tree) 0]
    [else (+ (count-nodes (bst-lbranch tree))
             (count-nodes (bst-rbranch tree)) 1)]))
(check-expect (count-nodes (add-to-tree BST7 (add-to-tree BST4 BST1))) 3)


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
            (purge (bst-lbranch tree) (bst-key tree))
            (purge (bst-rbranch tree) (bst-key tree))))


(define (purge tree n)
  ; BST Number -> BST
  ; delete node from tree
  (cond
    [(false? tree) #f]
    [(= (bst-key tree) n)
     (cond
       [(false? (bst-lbranch tree)) (bst-rbranch tree)]
       [(false? (bst-rbranch tree)) (bst-lbranch tree)]
       [else tree])]
    [(< n (bst-key tree))
     (make-bst (bst-key tree) (bst-val tree)
               (purge (bst-lbranch tree) n) (bst-rbranch tree))]
    [(> n (bst-key tree))
     (make-bst (bst-key tree) (bst-val tree)
               (bst-lbranch tree) (purge (bst-rbranch tree) n))]))
; checks
(check-expect (purge BST1 1) #f)
(check-expect (purge (add-to-tree BST1 BST3) 1) BST3)
(check-expect (purge (add-to-tree BST3 BST1) 1) BST3)
(check-expect (purge
               (add-to-tree (add-to-tree BST4 (add-to-tree BST1 BST3))
                            (add-to-tree BST7 BST5)) 1)
              (add-to-tree BST7 (add-to-tree BST4
                                             (add-to-tree BST3 BST5))))