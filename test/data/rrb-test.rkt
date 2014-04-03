#lang racket

(provide (all-defined-out) print-tree)

(require data/rrb)

(define tree1 (let loop ((i 2) (tree (create-tree 1)))
                   (if (< i 100) (loop (add1 i) (push i tree)) tree)))

(define tree2 (let loop ((i 101) (tree (create-tree 100)))
                   (if (< i 200) (loop (add1 i) (push i tree)) tree)))

(define tree3 (let loop ((i 201) (tree (create-tree 200)))
                   (if (< i 300) (loop (add1 i) (push i tree)) tree)))

(define tree4 (let loop ((i 301) (tree (create-tree 300)))
                   (if (< i 400) (loop (add1 i) (push i tree)) tree)))

(define tree12 (concat tree1 tree2))
(define tree23 (concat tree2 tree3))
(define tree34 (concat tree3 tree4))
(define tree1234a (concat tree12 tree34))
(define tree1234b (concat tree1 (concat tree23 tree4)))
(define treeread (get 227 tree1234a))
(define treewrite (set 227 1 tree1234a))

(define tree5 (let loop ((i 2) (tree (create-tree 1)))
                   (if (< i 10000) (loop (add1 i) (push i tree)) tree)))


(define tree6 (let loop ((i 2) (tree (create-tree 20001)))
                   (if (< i 20000) (loop (add1 i) (push i tree)) tree)))

