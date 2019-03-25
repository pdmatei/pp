The repo contains the examples shown at the Programming Paradigms lecture.

# Contents:

### Higher-order functions ###
```c03-v0.hs``` - The implementation of the cost function for linear regression (ML), using higher-order functions and the functional *pipeline* style 
  
To run c03-v01.hs you need to install:
  - gnuplot (a plotting app)
  - EasyPlot https://hub.darcs.net/scravy/easyplot (a small library which helps running gnuplot commands from Haskell)
  
To install EasyPlot you can run ''cabal install easyplot''. Cabal is a app designed for installing Haskell libraries, very similar in spirit to ''apt-get''.

### Abstract Datatypes ###
```c04-final.hs``` - The implementation of the polymorphic type ```(MExpr a)``` which allows us to encode constants, matrices and matrix expressions, together with the implementation of most of matrix operations (e.g. addition, multiplication) defined in the previous lecture; the implementation also contains a reordering of matrix operations and an efficient computation operation

```Trees.java``` - The implementation of the polymorphic (generic) Tree type in Java - useful for comparing the Haskell and Java type systems and polymorphism

### Classes ###
```c05-final.hs``` - Enrolling the type ```(MExpr a)``` and the type constructor ```MExpr``` in classes ```Show```, ```Eq``` and ```Functor```, respectively. This makes matrices (and matrix expressions) displayable and comparable. Finally, it makes matrices functors, which allows mapping functions over their elements. A more interesting strategy for displaying matrices (in latex format) has been implemented in the previous lecture
