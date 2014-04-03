#lang racket

(provide (all-defined-out))

;; An RRB-Tree has two distinct data types. A leaf which contains data as
;; an array in _data, and a height in _height, that is always 0. A node has in
;; addition its size table in _sizes, while _data contains an array of nodes or
;; leaves.

(require racket/trace)
(require racket/vector)
(require (for-syntax syntax/define))
(require (only-in racket/fixnum fxrshift))
(require (only-in racket/unsafe/ops unsafe-fxrshift))


(define-syntax (trace-define stx)
  (syntax-case stx ()
    [(_ e ...)
     (let-values ([(name def) (normalize-definition stx #'lambda)])
       #`(begin (define #,name #,def) (trace #,name)))]))

;; M is the maximal node size. 32 seems fast. E is the allowed increase
;; of search steps when concatting to find an index. Lower values will 
;; decrease balancing, but will increase search steps.
;; The user is responsible for two things:
;;   1. M MUST BE A POWER OF 2
;;   2. log2m be log_2 (m), which is a FIXNUM
(define m 4)
(define log2m 2)
(define e 2)

(struct node (height sizes data) #:transparent #:mutable)

(define index-sub
  (lambda (slot n)
    (if (zero? slot) 0 (vector-ref (node-sizes n) (sub1 slot)))))

;; Returns how many items are in the tree.

(define length
  (lambda (n)
    (if (leaf-node? n) 
        (vector-length (node-data n))
        (let ((s (node-sizes n)))
          (vector-ref s (sub1 (vector-length s)))))))

;; Copies a node for updating. Note that you should not use
;; this if only updating one of _data and _sizes for performance reasons.

(define node-copy
  (lambda (n)
    (if (leaf-node? n) 
      (node 0 #f (vector-copy (node-data n)))
      (node (node-height n)
            (vector-copy (node-sizes n)) 
            (vector-copy (node-data n))))))

(define leaf-node?
  (lambda (n)
    (zero? (node-height n))))

 ;; Gets the value at index i recursively.
(trace-define get
  (lambda (i n)
    (if (leaf-node? n) 
        (vector-ref (node-data n) i)
        (let ((slot (get-slot i n)))
          (get (- i (index-sub slot n)) 
               (vector-ref (node-data n) slot))))))

;; Calculates in which slot the item probably is, then
;; find the exact slot in the size table. Returns the index.
(define fast-shifts
  (lambda (n i) (if (zero? n) 
                    i 
                    (fast-shifts (sub1 n) (unsafe-fxrshift i log2m)))))

(trace-define get-slot
  (lambda (i n)
    (let ((ilst (node-sizes n)))
      (let loop ([slot (fast-shifts (node-height n) i)])
        (if (< i (vector-ref ilst slot)) slot (loop (add1 slot)))))))

;; Sets the value at the index i. Only the nodes leading to i will get
;; copied and updated.
(trace-define set
  (lambda (i item n)
    (let ((new (node-copy n)))
      (if (leaf-node? n)
          (begin (vector-set! (node-data new) i item) new)
          (let ((slot (get-slot i n)))
            (begin
              (vector-set! (node-data new) 
                           slot 
                           (set (- i (index-sub slot n))
                                item
                                (vector-ref (node-data n) slot)))
              new))))))

;; Vector Helpers
(define vector-last
  (lambda (vec)
    (let ((size (vector-length vec)))
      (vector-ref vec (sub1 size)))))

(define vector-set!-first
  (lambda (vec item) (vector-set! vec 0 item)))

(define vector-set!-last
  (lambda (vec item)
    (let ((size (vector-length vec)))
      (vector-set! vec (sub1 size) item))))

;; Pushes an item via pushloop to the bottom right of a tree.
(define push
  (lambda (item n)                           
  (define node-push
    (lambda (item n)
      (let ((sizes (node-sizes n)) (data (node-data n)) (height (node-height n)))
        (if (leaf-node? n)
          (node 
            height 
            #f 
            (vector-append data (vector item)))
          (node 
            height 
            (vector-append sizes (vector (+ (vector-length (node-data item)) (vector-last sizes))))
            (vector-append data (vector item)))))))
  ;; Recursively tries to push an item to the bottom-right most
  ;; tree possible. If there is no space left for the item,
  ;; null will be returned.
  (define pushloop
    (lambda (item n)
      (cond
        [(and (leaf-node? n) (< (vector-length (node-data n)) m))
         (node-push item n)]
        [(leaf-node? n) #f]
        [(pushloop item (botRight n)) => ;; Recursively Push!
         (lambda (pushed)                  ;; There was space in the bottom
           (let ((new (node-copy n)))   ;; right tree, so the slot will be 
             (vector-set!-last (node-data new) pushed) ;; updated.
             (vector-set!-last (node-sizes new) 
                          (add1 (vector-last (node-sizes new))))
             new))]
        [(< (vector-length (node-data n)) m) 
         (let          ;; When there was no space below, see if there is                                 
           ((new-slot  ;; space left for a new slot with the item at the bottom.
              (create item (sub1 (node-height n)))))
           (node-push new-slot n))]
        [else #f])))              
    (cond
      [(pushloop item n) => (lambda (pushed) pushed)]
      [else (siblise n (create item (node-height n)))])))

;; Concats two trees.
;; TODO: Add support for concatting trees of different sizes. Current
;; behavior will just rise the lower tree and then concat them.
(trace-define concat
  (lambda (node-a node-b)
    (let ((height-a (node-height node-a))
          (height-b (node-height node-b)))
      (cond
        [(> height-b height-a) (concat (parentise node-a height-b) node-b)]
        [(> height-a height-b) (concat node-a (parentise node-b height-a))]
        [(= height-a 0) (concat (parentise node-a 1) (parentise node-b 1)) ]
        [else 
          (let-values (((c0 c1) (concatloop node-a node-b)))
            (if c1 (siblise c0 c1) c0))]))))

;; Returns an array of two nodes. The second node _may_ be empty. This case
;; needs to be handled by the function, that called concat_. May be only
;; called for trees with an minimal height of 1.
(trace-define concatloop
  (lambda (a b)
    (trace-define node-drop
     (lambda (n)
       (node
         (node-height n)
         (vector-drop (node-data n) 1)
         (let ((size-disp (vector-ref (node-sizes n) 0)))
           (vector-map (lambda (v) (- v size-disp)) (vector-drop (node-sizes n) 1))))))
    (trace-define balance-recur
      (lambda (a b)
        (let ((toRemove (calc-to-remove a b)))
             (if (<= toRemove e) (values a b) (rebalance a b toRemove)))))
    (cond
      [(= 1 (node-height a)) (balance-recur a b)] ;; Check if balancing is needed and return based on that.
      [else 
        (let-values (((c0 c1) (concatloop (botRight a) (botLeft b))))
          (let ((a (node-copy a))
                (b (node-copy b)))
            (vector-set!-last (node-data a) c0)
            (let* ((s (node-sizes a))
                   (slen (vector-length s)))
              (vector-set!-last s (+ (length c0)
                                (if (> slen 1) (vector-ref s (- slen 2)) 0)))
              (printf "Moved stuff already~n")
              (cond
                [(zero? (vector-length (node-data c1)))
                 (let ((b (node-drop b)))
                   (if (zero? (vector-length (node-data b))) 
                       (values a b) 
                       (balance-recur a b)))]
                [else 
                  (let* ((bsize (node-sizes b))
                         (bdata (node-data b))
                         (blen (vector-length bsize))
                         (c1len (length c1)))
                    (begin
                      (vector-set!-first bdata c1)
                      (vector-set!-first (node-sizes b) c1len)
                      (let loop ((i 1) (len c1len)) 
                        (if (>= i blen) bsize (let ((len (+ len (length (vector-ref bdata i)))))
                                                (begin (vector-set! bsize i len) (loop (add1 i) len)))))
                      (balance-recur a b)))]))))])))

;; Returns the extra search steps for E. Refer to the paper.
(trace-define calc-to-remove 
  (lambda (a b)
    (letrec ((child-sum (lambda (vec) 
                          (let loop ((i 0) (sum 0))
                            (if (<= (vector-length vec) i) 
                                sum 
                                (loop (add1 i) (+ sum (vector-length (node-data (vector-ref vec i)))))))))
             (adata (node-data a))
             (bdata (node-data b))) 
      (let ((sublen (+ (child-sum adata) (child-sum bdata))))
        (- (+ (vector-length adata) (vector-length bdata))
           (add1 (quotient (sub1 sublen) m)))))))

;; get2 and set2 are helpers for accessing over two vectors
(define get2
  (lambda (vec-a vec-b index)
    (let ((len-a (vector-length vec-a)))
      (cond
        [(< index len-a) (vector-ref vec-a index)]
        [else (vector-ref vec-b (- index len-a))]))))

(define set2
  (lambda (vec-a vec-b index value)
    (let ((len-a (vector-length vec-a)))
      (cond
        [(< index len-a) (vector-set! vec-a index value)]
        [else (vector-set! vec-b (- index len-a) value)]))))

;; Creates a node or leaf with a given length at their arrays for perfomance.
;; Is only used by rebalance.
(trace-define create-len-node
  (lambda (height length)
    (let ((len (if (< length 0) 0 length)))
      (node height (if (zero? height) #f (make-vector length)) (make-vector length)))))

(define saveSlot
  (lambda (a b index slot)
    (set2 (node-data a) (node-data b) index slot)
    (let ((asizes (node-sizes a))
          (bsizes (node-sizes b)))
      (let ((l (if (or (zero? index) (= index (vector-length asizes))) 0 (get2 asizes asizes (sub1 index)))))
        (set2 asizes bsizes index (+ l (length slot)))))))

;; Returns an array of two balanced nodes.

(define node-data-length (lambda (n) (vector-length (node-data n))))
(define node-sizes-length (lambda (n) (vector-length (node-sizes n))))

(define vector-slice
  (lambda (vec from to)
    (vector-take (vector-drop vec from) (sub1 to))))

(trace-define rebalance
  (lambda (a b toRemove)
    (let* ((newA (create-len-node (node-height a) 
                    (min m (- (+ (node-data-length a) (node-data-length b)) toRemove))))
           (newB (create-len-node (node-height a) 
                    (- (node-data-length newA) (- (+ (node-data-length a) (node-data-length b)) toRemove))))
           (nadata (node-data newA))   (nbdata (node-data newB))
           (adata (node-data a))       (bdata (node-data b))
           (nasizes (node-sizes newA)) (nbsizes (node-sizes newB)) 
           (asizes (node-sizes a))     (bsizes (node-sizes b))
           (read ;; Skip the slots with size M. More precise: copy the slot references to the new node    
              (let loop ((read 0))
                (if (not (zero? (quotient (node-data-length (get2 (node-data a) (node-data b) read)) m))) read
                    (begin
                      (set2 nadata nbdata read (get2 adata bdata read))
                      (set2 nasizes nbsizes read (get2 asizes bsizes read))
                      (loop (add1 read))))))
           (write read)
           (slot (create-len-node (sub1 (node-height a)) 0))
           (from 0))
      (printf "~ngot past the let*~n")
      (let-values 
        (((write read slot )
           (let loop ((write read) (read read) (slot slot) (from from) (to 0))
           (if (< (- (- read write) (if (> (node-data-length slot) 0) 1 0)) toRemove)
               (values write read slot)
                (let* ((src (get2 adata bdata read))
                       (srcdata (node-data src))     (srcsize (node-sizes src))
                       (to (min (- m (node-data-length slot)) (node-data-length src))))
                  (set-node-data! slot (vector-append (vector-slice srcdata from to)))
                  (when (> 0 (node-height slot))
                    (let ((len (node-sizes-length slot))
                          (slot-data (node-data slot))
                          (slot-sizes (node-sizes slot)))
                      (let loop ((i len))
                        (when (< i (+ len (- to from)))
                           (begin
                             (vector-set! slot-sizes i 
                               (+ (vector-length (vector-ref slot-data i)) (if (zero? i) 0 (vector-ref slot-sizes (sub1 i)))))
                             (loop (add1 i)))))))
                  (let ((from (+ from to)))
                    (cond
                     [(<= (vector-length srcdata) to) (loop write (add1 read) slot 0 to)]
                     [(= (vector-length srcdata) m)
                      (begin
                       (saveSlot newA newB write slot)
                       (loop (add1 write) read (create-len-node (sub1 (node-height a)) from to)))]
                     [else (loop write read slot from to)])))))))
        (let ((write (if (> (node-data-length slot) 0)
                         (begin (saveSlot newA newB write slot) (add1 write))
                         write)))
        
          (let loop ((read read) (write write))
            (if (>= read (+ (node-data-length a) (node-data-length b)))
                (values newA newB)
                (begin (saveSlot newA newB write (get2 adata bdata read)) (loop (add1 read) (add1 write))))))))))

;;  ;; Pulling items from left to right, caching in a slot before writing
;;  ;; it into the new nodes.
;;  var write = read;
;;  var slot = new createNode(a._height - 1, 0);
;;  var from = 0;
;;  
;;  ;; If the current slot is still containing data, then there will be at
;;  ;; least one more write, so we do not break this loop yet.
;;  while (read - write - (slot._data.length > 0 ? 1 : 0) < toRemove) {
;;    ;; Find out the max possible items for copying.
;;    var source = get2(a._data, b._data, read);
;;    var to = Math.min(M - slot._data.length, source._data.length)
;;  
;;    ;; Copy and adjust size table.
;;    slot._data = slot._data.concat(source._data.slice(from, to));
;;    if (slot._height > 0) {
;;      var len = slot._sizes.length;
;;      for (var i = len ... i < len + to - from ... i++) {
;;        slot._sizes[i] = length(slot._data[i]);
;;        slot._sizes[i] += (i > 0 ? slot._sizes[i - 1] : 0);
;;      }
;;    }
;;  
;;    from += to;
;;  
;;    ;; Only proceed to next slots[i] if the current one was
;;    ;; fully copied.
;;    if (source._data.length <= to) {
;;      read++;
;;      from = 0;
;;    }
;;  
;;    ;; Only create a new slot if the current one is filled up.
;;    if (slot._data.length == M) {
;;      saveSlot(newA, newB, write, slot);
;;      slot = createNode(a._height - 1, 0);
;;      write++;
;;    }
;;  }
;;  
;;  
;;  
;;  // Cleanup after the loop. Copy the last slot into the new nodes.
;;  if (slot._data.length > 0) {
;;    saveSlot(newA, newB, write, slot);
;;    write++;
;;  }
;;  
;;  // Shift the untouched slots to the left
;;  while (read < a._data.length + b._data.length) {
;;    saveSlot(newA, newB, write, get2(a._data, b._data, read));
;;    read++;
;;    write++;
;;  }
;;  
;;  return [newA, newB];

;; Helper functions

(define botRight
  (lambda (n) 
    (let ((data (node-data n)))
      (vector-ref data (sub1 (vector-length data))))))

(define botLeft
  (lambda (n) (vector-ref (node-data n) 0)))

;; Recursively creates a tree with a given height containing
;; only the given item.
(define create
  (lambda (item height)
    (cond
      [(zero? height) (node 0 #f (vector item))]
      [else (node height (vector 1) (vector (create item (sub1 height))))])))

;; Recursively creates a tree that contains the given tree.
(define parentise
  (lambda (tree height)
    (if (= (node-height tree) height) 
        tree
        (node height (vector (length tree)) (vector (parentise tree (sub1 height)))))))

;; Emphasizes blood brotherhood beneath two trees.
(define siblise
  (lambda (a b)
    (node (add1 (node-height a)) (vector (length a) (+ (length a) (length b))) (vector a b))))


(define tree1 (let loop ((i 2) (tree (create 1 0)))
                   (if (< i 100) (loop (add1 i) (push i tree)) tree)))
(define tree2 (let loop ((i 101) (tree (create 100 0)))
                   (if (< i 200) (loop (add1 i) (push i tree)) tree)))
;; (define tree2 (let loop ((i 2) (tree (create 1 0)))
;;                   (if (< i 3) (loop (add1 i) (push i tree)) tree)))

(define printtree
  (lambda (n depth)
    (cond
      [(leaf-node? n) 
       (begin
         (let loop ((i 0)) (when (< i depth) (begin (printf "  ") (loop (add1 i)))))
         (printf "Data: ~s~n" (node-data n)))] 
      [else 
        (let loop ((i 0)) (when (< i depth) (begin (printf "  ") (loop (add1 i)))))
        (printf "Height: ~s " (node-height n))
        (let loop ((i 0)) (when (< i depth) (begin (printf "  ") (loop (add1 i)))))
        (printf "Ranges: ~s~n" (if (node-sizes n) (node-sizes n) 0))
        (vector-map (lambda (x) (printtree x (add1 (add1 depth)))) (node-data n)) (void)])))
      

