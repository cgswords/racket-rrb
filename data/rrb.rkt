#lang racket

(require racket/contract/base
         "rrb/fast.rkt")

(provide/contract 
  [rrb-ref          (exact-nonnegative-integer? rrb? . -> . any/c)]
  [rrb-set          (exact-nonnegative-integer? any/c rrb? . -> . rrb?)]
  [rrb-push         (any/c rrb? . -> . rrb?)]
  [rrb-concat       (rrb? rrb? . -> . rrb?)]
  [rrb-print-tree   (rrb? . -> . void?)]
  [rrb-count        (rrb? . -> . exact-nonnegative-integer?)]
  [make-rrb         (any/c . -> . rrb?)]
  [rrb?             (any/c . -> . boolean?)])


