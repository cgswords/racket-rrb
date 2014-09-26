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


    rrb-get         : Int -> RRB a -> a
    rrb-set         : Int -> a -> RRB a -> RRB a
    rrb-push        : a -> RRB a -> RRB a
    rrb-concat      : RRB a -> RRB a -> RRB a
    rrb-print-tree  : RRB a -> Void 
    rrb-length      : RRB a -> Int 
    rrb-create-tree : a -> RRB a
    rrb?            : any/c -> Bool

Each works about as expected; here, `set` is persistent (and thus sort of expensive).

### To Do

- Reimplement `concat` to be less lazy about how it concatenates trees.
- Document more of the code.
- Remove some of the excessive vector operations (such as `build-slot-sizes`).
- Contracts?
