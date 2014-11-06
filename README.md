hasler
======

Experimental distributed server for domain driven designed applications, supporting event sourcing, state-ful aggregate roots, CQRS etc. Built on Riak Core.

** Building Hasler

   Assuming you have a working Erlang (R16B03 - R17 is not supported due to Riak Core) installation,
   building Hasler should be as simple as:

#+BEGIN_EXAMPLE
   $ cd $HASLER
   $ make deps
   $ make
#+END_EXAMPLE
