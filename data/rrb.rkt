#lang racket

(require racket/contract/base
         "rrb/fast.rkt")

(define int? exact-nonnegative-integer?)

(provide/contract 
  [rrb-ref          (int? rrb? . -> . any/c)]
  [rrb-set          (int? any/c rrb? . -> . rrb?)]
  [rrb-push         (any/c rrb? . -> . rrb?)]
  [rrb-concat       (rrb? rrb? . -> . rrb?)]
  [rrb-print-tree   (rrb? . -> . void?)]
  [rrb-count        (rrb? . -> . int?)]
  [make-rrb         ((int?) (any/c) . ->* . rrb?)]
  [rrb?             (any/c . -> . boolean?)])


