PrePartiCL
==========
This repository holds a couple of code fragments that demonstrate a few of the techniques we plan to use with PartiCL.
The first demonstrates the use of Template Haskell to vectorize a Haskell record type.
That is, it takes a record type with scalar fields (instances of Data.Vector.Unbox) and returns a record type with vector fields.
The second is a simple example of using a GADT with an associated type to define a type class to perform a similar transformation.
