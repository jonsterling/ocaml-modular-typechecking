Inspired by a number of things:

1. Martin-Loef's old modular meaning explanations for extensional type theory
2. Conor McBride's code splicing trickery for solving the expression problem in
   his Epigram implementation.
3. Conor McBride's recent paper on encoding general recursive programs.
4. Bob Harper's remarks on using exceptions to achieve modularity (especially,
   exceptions as shared secrets).

Thanks to Joseph Abrahamson for working up an abstract binding trees
implementation for OCaml. I have inlined it here, but once I learn how to do
so, I'll simply depend upon it as a library.

What I have here is a (fragment of) a modular type checker. Each theory is
implemented in separate functor, and then they are all composed together at the
very end into a type checker. Because of the modular way I have set this up, it
is literally impossible for code from one theory to interfere with code from
another; yet they are cooperate seamlessly.
