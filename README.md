Current contents: <br/>
*ptest.sml* - contains bad recursive code to generate permutations
            in no particular order <br/>
*isprime.sml* - contains bad trial division code for primality testing <br/>
*bogosort.sml* - Uh <br/>
*subsetsum.sml* - inefficient subset sum that returns the subset too. but so pretty x.x <br/>
*powerset.sml* - two pieces of shitty code that both return the power set of a list... in different orders :P <br/>
*trees/btree.sig* - a signature for a BST <br/>
*trees/avltree.sml* - An AVL tree implementation of a self-balancing BST<br/>
*span.sml* - `span : ('a -> bool) -> 'a list -> 'a list * 'a list`<br/>
meant to behave like the Haskell list function span, more or less (at least, I hope it does!). Treat initials like really stupid test code for it.<br/>
*nqueens.sml* - contains a _slow_ N queens solver. Slow as in it literally tries all squares in sequence. Mainly there for fun with continuations and refs.
