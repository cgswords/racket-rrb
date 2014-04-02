#lang racket
 
(require racket/fixnum)

(provide length index t0 t1)

(struct node (ranges children))
(struct leaf (vec))
(struct nil (nil))

(define m 4)
;; (define m 32)

(define length
  (lambda (node)
    (cond
     [(nil? node) 0]
     [(leaf? node) (vector-length (leaf-vec node))]
     [(node? node) (vector-ref 
                     (node-ranges node)
                     (sub1 (vector-length (node-ranges node))))])))

(define index 
  (lambda (node i)
    (cond
      [(nil? node)  (error "Invalid index: " i)]
      [(leaf? node) (vector-ref (leaf-vec node) i)]
      [(node? node) 
       (let ((ilst (node-ranges node))
             (alst (node-children node)))
         (let loop ([slot (fxquotient i m)])
           (let ((valid-index (< i (vector-ref ilst slot)))
                 (szero       (zero? slot)))
             (cond
               [(and valid-index szero) (index (vector-ref alst slot) i)]
               [valid-index (index (vector-ref alst slot)
                                   (- i (vector-ref ilst (sub1 slot))))]
               [else (loop (add1 slot))]))))]
      [else (error "Invalid node: " node)]         )))

(define t0 (leaf '#(8 10 12 14)))

(define t1
  (let ((l1 (leaf '#(2 4 6)))
        (l2 (leaf '#(8 10 12 14)))
        (l3 (leaf '#(16 18 20 22))))
    (node '#(3 7 11) (vector l1 l2 l3))))

;; index  :: (Show a) => RRBVector a -> Int -> a
;; index (Leaf e) i = (V.!) e i
;; index n@(Node il al) i =
;;   loop $ div i m
;;   where 
;;     loop s = if i < ((V.!) il s)
;;              then 
;;                if (s == 0)
;;                then index ((V.!) al s) i
;;                else index ((V.!) al s) (i - ((V.!) il (s - 1)))
;;              else loop $ s + 1



;; rebalance :: RRBVector a -> RRBVector b -> 
;; rebalance n1 Nil = n1 
;; rebalance n1 n2 =
;;   let slc1 = (slot-count nm am n1 shift)
;;       slc2 = 
;;       slc2 = (slot-count nm am n2 shift)
;;       a    = (+ slc1 slc2)
;;       sbc1 = (subtree-branch-count nm am n1 shift)
;;       sbc2 = (subtree-branch-count nm am n2 shift)
;;       p    = (+ sbc1 sbc2)
;;       e    = (- a (inc (quot (dec p) m)))]
;;   if e <= max-extra-search-steps
;;   then (object-array (list n1 n2))
;;   else if (sbc1 + sbc2) <= 1024
;;   then let new_arr  = replicate m Nil
;;            new_rngs = replicate m 0
;;            new_n1   = Node empty new_arr
;;            i        = 0
;;            b:bs    = partitionAll 32 (concat (childSeq n1 shift cnt1) (childSeq m n2 shift cnt2))
;;             (when-first [block bs]
;;               (let [a (object-array 33)
;;                     r (int-array 33)]
;;                 (aset a 32 r)
;;                 (aset r 32 (count block))
;;                 (loop [i 0 o (int 0) gcs (seq block)]
;;                   (when-first [[gc gcr] gcs]
;;                     (aset ^objects a i gc)
;;                     (aset r i (unchecked-add-int o (int gcr)))
;;                     (recur (inc i) (unchecked-add-int o (int gcr)) (next gcs))))
;;                 (aset ^objects new-arr i (.node nm nil a))
;;                 (aset new-rngs i
;;                       (+ (aget r (dec (aget r 32)))
;;                          (if (pos? i) (aget new-rngs (dec i)) (int 0))))
;;                 (aset new-rngs 32 (inc i))
;;                 (recur (inc i) (next bs))))
;;            loop      = \ i bs -> (aset new-arr 32 new-rngs)
;;                                  (set! (.-val transferred-leaves) cnt2)
;;                                  (object-array (list new-n1 nil))
;;            loop i (b:bs)
;;   else let new_arr1  = replicate m Nil
;;            new_arr2  = replicate m Nil
;;            new_rngs1 = replicate m 0
;;            new_rngs2 = replicate m 0
;;            new_n1    = Node empty new_arr1
;;            new_n2    = Node empty new_arr2
;;        (loop [i  0
;;                  bs (partition-all 32
;;                                    (concat (child-seq nm n1 shift cnt1)
;;                                            (child-seq nm n2 shift cnt2)))]
;;             (when-first [block bs]
;;               (let [a (object-array 33)
;;                     r (int-array 33)]
;;                 (aset a 32 r)
;;                 (aset r 32 (count block))
;;                 (loop [i 0 o (int 0) gcs (seq block)]
;;                   (when-first [[gc gcr] gcs]
;;                     (aset a i gc)
;;                     (aset r i (unchecked-add-int o (int gcr)))
;;                     (recur (inc i) (unchecked-add-int o (int gcr)) (next gcs))))
;;                 (if (and (< i 32) (> (+ (* i 32) (count block)) sbc1))
;;                   (let [tbs (- (+ (* i 32) (count block)) sbc1)
;;                         li  (dec (aget r 32))
;;                         d   (if (>= tbs 32)
;;                               (aget r li)
;;                               (- (aget r li) (aget r (- li tbs))))]
;;                     (set! (.-val transferred-leaves)
;;                           (+ (.-val transferred-leaves) d))))
;;                 (let [new-arr  (if (< i 32) new-arr1 new-arr2)
;;                       new-rngs (if (< i 32) new-rngs1 new-rngs2)
;;                       i        (mod i 32)]
;;                   (aset ^objects new-arr i (.node nm nil a))
;;                   (aset new-rngs i
;;                         (+ (aget r (dec (aget r 32)))
;;                            (if (pos? i) (aget new-rngs (dec i)) (int 0))))
;;                   (aset new-rngs 32 (int (inc i))))
;;                 (recur (inc i) (next bs)))))
;;           (aset new-arr1 32 new-rngs1)
;;           (aset new-arr2 32 new-rngs2)
;;           (object-array (list new-n1 new-n2)))))))
;; 
;; 
;; append :: RRBVector a -> RRBVector a -> RRBVector a
;; append t1 t2 = appendHelper t1 t2 [] []
;;   where
;;     leaves (Node il ((Leaf e1):n)) = True
;;     leaves _ = False
;;     findLHS (Leaf e) n i = (Leaf e, n, i+1)
;;     findLHS node n     i = findLHS (head al) (node:n) (i+1)
;;     findRHS (Leaf e) n i = (Leaf e, n, i+1)
;;     findRHS node n     i = findRHS (last al) (node:n) (i+1)
;;     appendHelper t1 t2 =
;;       let (rhs, rhsPath, height) = findRHS t1 [] 0
;;           (lhs, lhsPath, height) = findLHS t2 [] 0
;;       in 
;;       
  
