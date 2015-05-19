#lang racket/base

(provide 
  rrb-ref          ;; RRB a -> Int -> a
  rrb-set          ;; RRB a -> Int -> a -> RRB a
  rrb-push         ;; RRB a -> a -> RRB a
  rrb-concat       ;; RRB a -> RRB a -> RRB a
  rrb-print-tree   ;; RRB a -> Void 
  rrb-count        ;; RRB a -> Int 
  make-rrb         ;; Int -> RRB Int \/ Int -> a -> RRB a
  rrb?             ;; any -> Bool
  sliceRight)

;; An RRB-Tree has two distinct data types. A leaf which contains data as
;; an array in _data, and a height in _height, that is always 0. A rrb-node has in
;; addition its size table in _sizes, while _data contains an array of rrb-nodes or
;; leaves.

(require racket/vector
         racket/match
         (only-in racket/unsafe/ops unsafe-fxrshift))

;; M is the maximal rrb-node size. 32 seems fast. E is the allowed increase
;; of search steps when concatting to find an index. Lower values will 
;; decrease balancing, but will increase search steps.
;; The user is responsible for two things:
;;   1. M MUST BE A POWER OF 2
;;   2. log2m be log_2 (m), which is a FIXNUM
(define m 32)
(define log2m 5)
(define e 2)

(struct rrb-node (height sizes data)
  #:transparent #:mutable)

(define rrb? rrb-node?)

 ;; Gets the value at index i recursively.
(define (rrb-ref n i)
  (if (leaf-rrb-node? n) 
      (vector-ref (rrb-node-data n) i)
      (let ([slot (get-slot n i)])
        (rrb-ref (vector-ref (rrb-node-data n) slot)
                 (- i (index-sub slot n))))))

;; Sets the value at the index i. Only the rrb-nodes leading to i will get
;; copied and updated.
(define (rrb-set n i item)
  (define new (rrb-node-copy n))
  (if (leaf-rrb-node? n)
      (vector-set! (rrb-node-data new) i item)
      (let ([slot (get-slot n i)])
        (vector-set! (rrb-node-data new) 
                     slot 
                     (rrb-set (vector-ref (rrb-node-data n) slot)
                              (- i (index-sub slot n))
                              item))))
  new)

;; Pushes an item via pushloop to the bottom right of a tree.
(define (rrb-push n item)
  (define (rrb-node-push item n)
    (match-define (rrb-node height sizes data) n)
    (if (leaf-rrb-node? n)
        (rrb-node
         height
         #f
         (vector-append data (vector item)))
        (rrb-node 
         height 
         (vector-append sizes (vector (+ (vector-length (rrb-node-data item)) 
                                         (vector-last sizes))))
         (vector-append data (vector item)))))
  ;; Recursively tries to push an item to the bottom-right most
  ;; tree possible. If there is no space left for the item,
  ;; null will be returned.
  (define (pushloop item n)
    (cond
      [(and (leaf-rrb-node? n) (< (vector-length (rrb-node-data n)) m))
       (rrb-node-push item n)]
      [(leaf-rrb-node? n) #f]
      [(pushloop item (botRight n))
       =>                                                ;; Recursively Push!
       (lambda (pushed)                                  ;; There was space in the bottom
         (let ([new (rrb-node-copy n)])                  ;; right tree, so the slot will be 
           (vector-set!-last (rrb-node-data new) pushed) ;; updated.
           (vector-set!-last (rrb-node-sizes new) 
                             (add1 (vector-last (rrb-node-sizes new))))
           new))]
      [(< (vector-length (rrb-node-data n)) m)
       ;; When there was no space below, see if there is
       ;; space left for a new slot with the item at the bottom.
       (define new-slot (create item (sub1 (rrb-node-height n))))
       (rrb-node-push new-slot n)]
      [else #f]))
  (or (pushloop item n)
      (siblise n (create item (rrb-node-height n)))))

;; Print some stuff
(define (rrb-print-tree tree)
  (let recur ([n tree]
              [depth 0])
    (define (print-indentation)
      (for ([i (in-range depth)])
        (display "  ")))
    (cond
      [(leaf-rrb-node? n)
       (print-indentation)
       (printf "Data: ~s~n" (rrb-node-data n))] 
      [else 
       (print-indentation)
       (printf "Height: ~s " (rrb-node-height n))
       (print-indentation)
       (printf "Ranges: ~s~n" (if (rrb-node-sizes n) (rrb-node-sizes n) 0))
       (for ([x (in-vector (rrb-node-data n))])
         (recur x (+ 2 depth)))])))
 

;; Returns how many items are in the tree.
(define (rrb-count n)
  (if (leaf-rrb-node? n) 
      (vector-length (rrb-node-data n))
      (let ([s (rrb-node-sizes n)])
        (vector-ref s (sub1 (vector-length s))))))

;; Concats two trees.
;; TODO: Add support for concatting trees of different sizes. Current
;; behavior will just rise the lower tree and then concat them.
(define (rrb-concat rrb-node-a rrb-node-b)
  ;; Returns an array of two rrb-nodes. The second rrb-node _may_ be empty. This case
  ;; needs to be handled by the function, that called concat_. May be only
  ;; called for trees with an minimal height of 1.
  (define (concatloop a b)
    (define (rrb-node-drop n)
      (rrb-node
       (rrb-node-height n)
       (vector-drop (rrb-node-data n) 1)
       (let ([size-disp (vector-ref (rrb-node-sizes n) 0)])
         (vector-map (lambda (v) (- v size-disp))
                     (vector-drop (rrb-node-sizes n) 1)))))
    
    (define (balance-recur a b)
      (let ([toRemove (calc-to-remove a b)])
        (if (<= toRemove e)
            (values a b)
            (rebalance a b toRemove))))
    
    (cond
      [(= 1 (rrb-node-height a)) (balance-recur a b)] 
      ;; Check if balancing is needed and return based on that.
      [else 
       (let-values ([(c0 c1) (concatloop (botRight a) (botLeft b))])
         (let ([a (rrb-node-copy a)]
               [b (rrb-node-copy b)])
           (vector-set!-last (rrb-node-data a) c0)
           (let* ([s (rrb-node-sizes a)]
                  [slen (vector-length s)])
             (vector-set!-last s (+ (rrb-count c0)
                                    (if (> slen 1) (vector-ref s (- slen 2)) 0)))
             (cond
               [(zero? (vector-length (rrb-node-data c1)))
                (let ([b (rrb-node-drop b)])
                  (if (zero? (vector-length (rrb-node-data b))) 
                      (values a b) 
                      (balance-recur a b)))]
               [else 
                (let* ([bsize (rrb-node-sizes b)]
                       [bdata (rrb-node-data b)]
                       [blen (vector-length bsize)]
                       [c1len (rrb-count c1)])
                  (vector-set!-first bdata c1)
                  (vector-set!-first (rrb-node-sizes b) c1len)
                  (for/fold ([len c1len])
                            ([i (in-range 1 blen)])
                    (define new-len (+ len (rrb-count (vector-ref bdata i))))
                    (vector-set! bsize i new-len)
                    new-len)
                  (balance-recur a b))]))))]))
  
  ;; Returns the extra search steps for E. Refer to the paper.
  (define (calc-to-remove a b)
    (define (child-sum vec)
      (for/sum ([e (in-vector vec)])
        (vector-length (rrb-node-data e))))
    (let* ([adata (rrb-node-data a)]
           [bdata (rrb-node-data b)]
           [sublen (+ (child-sum adata) (child-sum bdata))])
      (- (+ (vector-length adata) (vector-length bdata))
         (add1 (quotient (sub1 sublen) m)))))
  
  (let ([height-a (rrb-node-height rrb-node-a)]
        [height-b (rrb-node-height rrb-node-b)])
    (cond
      [(> height-b height-a) (rrb-concat (parentise rrb-node-a height-b) rrb-node-b)]
      [(> height-a height-b) (rrb-concat rrb-node-a (parentise rrb-node-b height-a))]
      [(= height-a 0) (rrb-concat (parentise rrb-node-a 1) (parentise rrb-node-b 1)) ]
      [else 
       (let-values ([(c0 c1) (concatloop rrb-node-a rrb-node-b)])
         (if (> (rrb-node-data-length c1) 0)
             (siblise c0 c1) c0))])))

(define make-rrb 
  (case-lambda
    [(n) (make-rrb n 0)]
    [(n a)
     (for/fold ([t (create a 0)])
               ([i (in-range 1 n)]) ;; counter starts at 1 because we do it once to kick off the loop 
       (rrb-push t a))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get2 and set2 are helpers for accessing over two vectors
(define (get2 vec-a vec-b index)
  (let ([len-a (vector-length vec-a)])
    (if (< index len-a)
        (vector-ref vec-a index)
        (vector-ref vec-b (- index len-a)))))

(define (set2 vec-a vec-b index value)
  (let ([len-a (vector-length vec-a)])
    (if (< index len-a)
        (vector-set! vec-a index value)
        (vector-set! vec-b (- index len-a) value))))

;; Creates a rrb-node or leaf with a given length at their arrays for perfomance.
(define (create-len-rrb-node height length)
  (let ([len (max 0 length)])
    (rrb-node height
              (if (zero? height) #f (make-vector length))
              (make-vector length))))

(define (saveSlot a b index slot)
  (set2 (rrb-node-data a) (rrb-node-data b) index slot)
  (let ([asizes (rrb-node-sizes a)]
        [bsizes (rrb-node-sizes b)])
    (let ([l (if (or (zero? index) (= index (vector-length asizes)))
                 0 (get2 asizes asizes (sub1 index)))])
      (set2 asizes bsizes index (+ l (rrb-count slot))))))

(define (rebalance a b toRemove)
  (define (build-slot-sizes data)
    (let* ([dlen (vector-length data)]
           [new-slots (make-vector dlen 0)])
      (for ([i (in-range dlen)])
        (vector-set! new-slots i
                     (+ (rrb-count (vector-ref data i)) 
                        (if (zero? i) 0 (vector-ref new-slots (sub1 i))))))
      new-slots))
  
  (define (copy-elements src dest from to)
    (set-rrb-node-data! dest (vector-append (rrb-node-data dest)
                                            (vector-copy (rrb-node-data src) from to)))
    (unless (leaf-rrb-node? dest)
      (set-rrb-node-sizes! dest (build-slot-sizes (rrb-node-data dest)))))
  
  (let* ([newA (create-len-rrb-node (rrb-node-height a) 
                                    (min m (- (+ (rrb-node-data-length a)
                                                 (rrb-node-data-length b))
                                              toRemove)))]
         [newB (create-len-rrb-node (rrb-node-height a) 
                                    (- (rrb-node-data-length newA) (- (+ (rrb-node-data-length a)
                                                                         (rrb-node-data-length b))
                                                                      toRemove)))]
         ;; Skip the slots with size M. More precisely: copy the slot references to the new rrb-node
         [read*
          (or (for/last ([read (in-naturals)])
                #:break (not (zero? (modulo (rrb-node-data-length (get2 (rrb-node-data a)
                                                                        (rrb-node-data b)
                                                                        read))
                                            m)))
                (set2 (rrb-node-data newA) (rrb-node-data newB) read
                      (get2 (rrb-node-data a) (rrb-node-data b) read))
                (set2 (rrb-node-sizes newA) (rrb-node-sizes newB) read
                      (get2 (rrb-node-sizes a) (rrb-node-sizes b) read))
                (add1 read))
              0)])
    (define-values (write read slot)
      (let loop ([write read*]
                 [read read*]
                 [slot (create-len-rrb-node (sub1 (rrb-node-height a)) 0)]
                 [from 0]
                 [to 0])
        (if (< (- (- read write) (if (> (rrb-node-data-length slot) 0) 1 0)) toRemove)
            (let* ([src (get2 (rrb-node-data a) (rrb-node-data b) read)]
                   [to  (min (- m (rrb-node-data-length slot)) (rrb-node-data-length src))])
              (copy-elements src slot from to)
              (let* ([from (+ from to)]
                     [read (if (<= (rrb-node-data-length src) to)
                               (add1 read) read)]
                     [from (if (<= (rrb-node-data-length src) to)
                               0 from)]
                     [slot (if (=  (rrb-node-data-length slot) m) 
                               (begin (saveSlot newA newB write slot)
                                      (create-len-rrb-node (sub1 (rrb-node-height a)) 0))
                               slot)]
                     [write (if (= (rrb-node-data-length slot) m)
                                (add1 write) write)])
                (loop write read slot from to)))
            (values write read slot))))
    (let ([write (if (> (rrb-node-data-length slot) 0)
                     (begin (saveSlot newA newB write slot) (add1 write))
                     write)])
      (for ([read (in-naturals read)]
            [write (in-naturals write)])
        #:break (>= read (+ (rrb-node-data-length a) (rrb-node-data-length b)))
        (saveSlot newA newB write (get2 (rrb-node-data a) (rrb-node-data b) read)))
      (values newA newB))))



;; Calculates in which slot the item probably is, then
;; find the exact slot in the size table. Returns the index.
(define (fast-shifts n i)
  (if (zero? n)
      i
      (fast-shifts (sub1 n) (unsafe-fxrshift i log2m))))

(define (get-slot n i)
  (let ([ilst (rrb-node-sizes n)])
    (for/last ([slot (in-naturals (fast-shifts (rrb-node-height n) i))])
      #:final (< i (vector-ref ilst slot))
      slot)))

(define (botRight n)
  (let ([data (rrb-node-data n)])
    (vector-ref data (sub1 (vector-length data)))))

(define (botLeft n)
  (vector-ref (rrb-node-data n) 0))

(define (index-sub slot n)
  (if (zero? slot) 0
      (vector-ref (rrb-node-sizes n) (sub1 slot))))

(define (leaf-rrb-node? n)
  (zero? (rrb-node-height n)))

;; Copies a rrb-node for updating. Note that you should not use
;; this if only updating one of _data and _sizes for performance reasons.
(define (rrb-node-copy n)
  (if (leaf-rrb-node? n) 
      (rrb-node 0 #f (vector-copy (rrb-node-data n)))
      (rrb-node (rrb-node-height n)
                (vector-copy (rrb-node-sizes n)) 
                (vector-copy (rrb-node-data n)))))

(define (rrb-node-data-length n)  (vector-length (rrb-node-data n)))
(define (rrb-node-sizes-length n) (vector-length (rrb-node-sizes n)))

;; Vector Helpers
(define (vector-last vec)
  (let ([size (vector-length vec)])
    (vector-ref vec (sub1 size))))

(define (vector-set!-first vec item)
  (vector-set! vec 0 item))

(define (vector-set!-last vec item)
  (let ([size (vector-length vec)])
    (vector-set! vec (sub1 size) item)))

;; Recursively creates a tree with a given height containing
;; only the given item.
(define (create item height)
  (if (zero? height)
      (rrb-node 0 #f (vector item))
      (rrb-node height (vector 1) (vector (create item (sub1 height))))))

;; Recursively creates a tree that contains the given tree.
(define (parentise tree height)
  (if (= (rrb-node-height tree) height) 
      tree
      (rrb-node height (vector (rrb-count tree)) (vector (parentise tree (sub1 height))))))

;; C'mon, get together!
(define (siblise a b)
  (rrb-node (add1 (rrb-node-height a))
            (vector (rrb-count a)
                    (+ (rrb-count a) (rrb-count b)))
            (vector a b)))


;; This takes the right slice of the tree: it removes anything from the tree beyond index i
;; Does not maintain a balanced tree
(define (sliceRight tree index)
  (match-define (rrb-node height sizes data) tree)
  (cond
    [(leaf-rrb-node? tree) (rrb-node 0 #f (vector-take data index))]
    [else 
     (let* ([sliceIndex    (get-slot tree index)]
            [newSlicedNode (sliceRight
                            (vector-ref data sliceIndex)
                            (- index (index-sub sliceIndex index)) )]
            [newSizes (vector-take sizes (add1 sliceIndex))]
            [newData  (vector-take data (add1 sliceIndex))])
       (vector-set! newSizes sliceIndex index)
       (vector-set! newData  sliceIndex newSlicedNode)
       (rrb-node height newSizes newData))]))

;; (trace get-slot)
;; (trace sliceRight)

;; This takes the left slice of the tree: it removes anything from the tree before index i
;; Does not maintain a balanced tree
;; (define sliceLeft
;;   (lambda (index tree)
;;     (match tree
;;       [(rrb-node height sizes data) 
;;        (cond
;;          [(leaf-rrb-node? tree) (rrb-node 0 #f (vector-take-right data index))]
;;          [else
;;           (let* ((sliceIndex (get-slot index tree))
;;                  (newSlicedNode (sliceLeft (- i (index-sub slot index)) (vector-ref data slot))))
;;             (rrb-node height
;;                       (recompute-sizes sizes index)
;;                       (vector-set! (vector-take-right data index) index newSlicedNode)))])])))

;; (define rrb-ref
;;   (lambda (i n)
;;     (if (leaf-rrb-node? n) 
;;         (vector-ref (rrb-node-data n) i)
;;         (let ((slot (get-slot i n)))
;;           (rrb-ref (- i (index-sub slot n)) 
;;                (vector-ref (rrb-node-data n) slot))))))


