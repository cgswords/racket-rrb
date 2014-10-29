#lang racket

(require racket/contract/base
         "rrb/fast.rkt")

(define int? exact-nonnegative-integer?)

(provide/contract 
  [rrb-ref          (rrb? int? . -> . any/c)]
  [rrb-set          (rrb? int? any/c . -> . rrb?)]
  [rrb-push         (rrb? any/c . -> . rrb?)]
  [rrb-concat       (rrb? rrb? . -> . rrb?)]
  [rrb-print-tree   (rrb? . -> . void?)]
  [rrb-count        (rrb? . -> . int?)]
  [make-rrb         ((int?) (any/c) . ->* . rrb?)]
  [rrb?             (any/c . -> . boolean?)]
  ;; [sliceRight       (rrb? int? . -> . rrb?)]
  )


