# racket-rrb

Racket RRB Tree Implementation (current WIP)

### To Install:

    $ raco pkg install git://github.com/cgswords/racket-rrb
    
or

    $ git clone git://github.com/cgswords/racket-rrb
    $ cd racket-rrb
    $ raco pkg install

### To Do

- Reimplement `concat` to be less lazy about how it concatenates trees.
- Document more of the code.
- Remove some of the excessive vector operations (such as `build-slot-sizes`).
