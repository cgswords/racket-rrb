# racket-rrb

Racket RRB Tree Implementation (current WIP)

### To Install:

    $ raco pkg install git://github.com/cgswords/racket-rrb
    
or

    $ git clone git://github.com/cgswords/racket-rrb
    $ cd racket-rrb
    $ raco pkg install
    
### Usage

The structure provides the following functions:

    rrb-ref          ;; RRB a -> Int -> a
    rrb-set          ;; RRB a -> Int -> a -> RRB a
    rrb-push         ;; RRB a -> a -> RRB a
    rrb-concat       ;; RRB a -> RRB a -> RRB a
    rrb-print-tree   ;; RRB a -> Void 
    rrb-count        ;; RRB a -> Int 
    make-rrb         ;; Int -> RRB Int \/ Int -> a -> RRB a
    rrb?             ;; any -> Bool

Each works about as expected; here, `set` is persistent (and thus sort of expensive).

### To Do

- Reimplement `concat` to be less lazy about how it concatenates trees.
- Document more of the code.
- Remove some of the excessive vector operations (such as `build-slot-sizes`).
- Contracts?
