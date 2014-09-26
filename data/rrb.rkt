#lang racket

(require racket/contract/base
         "rrb/fast.rkt")

(provide/contract 
  [rrb-get          (integer? rrb? . -> . any/c)]
  [rrb-set          (integer? any/c rrb? . -> . rrb?)]
  [rrb-push         (any/c rrb? . -> . rrb?)]
  [rrb-concat       (rrb? rrb? . -> . rrb?)]
  [rrb-print-tree   (rrb? . -> . void?)]
  [rrb-length       (rrb? . -> . integer?)]
  [rrb-create-tree  (any/c . -> . rrb?)]
  [rrb?             (any/c . -> . boolean?)])


