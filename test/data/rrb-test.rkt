#lang racket

(require data/rrb)
(require rackunit)

(define tree1 (let loop ((i 2) (tree (make-rrb 1)))
                   (if (< i 100) (loop (add1 i) (rrb-push i tree)) tree)))

(define tree2 (let loop ((i 101) (tree (make-rrb 100)))
                   (if (< i 200) (loop (add1 i) (rrb-push i tree)) tree)))

(define tree3 (let loop ((i 201) (tree (make-rrb 200)))
                   (if (< i 300) (loop (add1 i) (rrb-push i tree)) tree)))

(define tree4 (let loop ((i 301) (tree (make-rrb 300)))
                   (if (< i 400) (loop (add1 i) (rrb-push i tree)) tree)))

(define tree5 (let loop ((i 2) (tree (make-rrb 1)))
                   (if (< i 10000) (loop (add1 i) (rrb-push i tree)) tree)))

(define tree6 (let loop ((i 10001) (tree (make-rrb 20001)))
                   (if (< i 20000) (loop (add1 i) (rrb-push i tree)) tree)))

(define tree12 (rrb-concat tree1 tree2))
(define tree23 (rrb-concat tree2 tree3))
(define tree34 (rrb-concat tree3 tree4))
(define tree1234a (rrb-concat tree12 tree34))
(define tree1234b (rrb-concat tree1 (rrb-concat tree23 tree4)))

(define tree56 (rrb-concat tree5 tree6))

(define rrb-set-pairs
  (lambda (ls tree)
    (cond
      [(null? ls) tree]
      [else (rrb-set-pairs (cdr ls) (rrb-set (caar ls) (cdar ls) tree))])))

(define rrb-test
  (test-suite "Tests for rrb.rkt" 
    (check-equal?
      (for/list ([i (in-range 0 99)]) (rrb-ref i tree1))
      (for/list ([i (in-range 1 100)]) i)
      "Tree Get Test #1")

    (check-equal?
      (for/list ([i (in-range 0 99)]) (rrb-ref i tree2))
      (for/list ([i (in-range 100 199)]) i)
      "Tree Get Test #2")

    (check-equal?
      (for/list ([i (in-range 0 99)]) (rrb-ref i tree3))
      (for/list ([i (in-range 200 299)]) i)
      "Tree Get Test #3")

    (check-equal?
      (for/list ([i (in-range 0 99)]) (rrb-ref i tree4))
      (for/list ([i (in-range 300 399)]) i)
      "Tree Get Test #4")

    (check-equal?
      (for/list ([i (in-range 1000 1999)]) (rrb-ref i tree5))
      (for/list ([i (in-range 1001 2000)]) i)
      "Tree Get Test #5")

    (check-equal?
      (for/list ([i (in-range 1000 1999)]) (rrb-ref i tree6))
      (for/list ([i (in-range 11000 11999)]) i)
      "Tree Get Test #6")

    (check-equal?
      (for/list ([i (in-range 0 199)]) (rrb-ref i tree12))
      (for/list ([i (in-range 1 200)]) i)
      "Tree Concat Test #1")

    (check-equal?
      (for/list ([i (in-range 0 199)]) (rrb-ref i tree23))
      (for/list ([i (in-range 100 299)]) i)
      "Tree Concat Test #2")

    (check-equal?
      (for/list ([i (in-range 0 199)]) (rrb-ref i tree34))
      (for/list ([i (in-range 200 399)]) i)
      "Tree Concat Test #3")

    (check-equal?
      (for/list ([i (in-range 0 399)]) (rrb-ref i tree1234a))
      (for/list ([i (in-range 1 400)]) i)
      "Tree Concat Test #4")

    (check-equal?
      (for/list ([i (in-range 0 399)]) (rrb-ref i tree1234b))
      (for/list ([i (in-range 1 400)]) i)
      "Tree Concat Test #5")

    (check-equal?
      (for/list ([i (in-range 0 19999)]) (rrb-ref i tree56))
      (append (for/list ([i (in-range 0 9999)]) (rrb-ref i tree5)) (for/list ([i (in-range 0 10000)]) (rrb-ref i tree6)))
      "Tree Concat Test #6")

    (check-equal?
      (let ((tree (rrb-set-pairs (for/list ([i (in-range 0 399)]) (cons i 0)) tree1234a)))
        (for/list ([i (in-range 0 399)]) (rrb-ref i tree)))
      (for/list ([i (in-range 0 399)]) 0)
      "Tree Set Test")))

    (check-equal?
      (let* ((tree (rrb-set-pairs (for/list ([i (in-range 0 375)]) (cons i 0)) tree1234a))
             (tree1 (rrb-push 50 tree))
             (tree2 (rrb-push 50 tree)))
        (eq? tree1 tree2))
      #f
      "Persistance check 1")

    (check-equal?
      (let ((tree1 (rrb-concat tree1234a tree1234a))
            (tree2 (rrb-concat tree1234a tree1234a)))
        (eq? tree1 tree2))
      #f
      "Persistance check 2")

(require rackunit/text-ui)
(run-tests rrb-test)
