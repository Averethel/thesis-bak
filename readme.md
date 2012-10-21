Proof Carrying Code for MiniML
==============================
The idea is to create PCC framework for MiniML - subset of OCaml Light language.

Current status
==============
Implementing MiniML compiler to ZINC bytecode

Currently working on
====================
* Rewriting Enriched Lambda Calculus

Changelog
=========
* 21 X 2012
    * [Languages]
        * Added head, tail, empty?, fst and snd primitives to MiniML
    * [Compiler]
        * Initial version of pattern matching translations
* 10 IX 2012
    * [Compiler]
        * Changed compiler structure
        * Added folding tuples
        * Added wildcard pattern removing
* 9 IX 2012
    * [Languages]
        * Deleted Enriched Lambda Calculus With Low Level Types
        * Rewrote Enriched Lambda Calculus syntax to be similar to Core language described in Implementing Functional Languages: a tutorial
* 18 VIII 2012
    * [Languages] 
        * MiniML interpreter reimplemented using monads
        * Mutually recursive letrec in Enriched Lambda Calculus With Low Level Types
        * Added definitions to Enriched Lambda Calculus With Low Level Types
    * [Compiler]
        * Finished transaltion from Enriched Lambda Calculus to Enriched Lambda Calculus With Low Level Types
    * [Others]
        * Enriched Lambda Calculus interpreter now uses record as sate
* 15 VIII 2012
    * [Compiler]
        * Finished transaltion from MiniML to Enriched Lambda Calculus
* 11 VIII 2012
    * [Languages]
        * Mutually recursive letrec in Enriched Lambda Calculus
    * [Compiler]
        * Enforcing unique variable naming
*  7 VIII 2012
    * [Languages]
        * Added definitions to Enriched Lambda Calculus
*  6 VIII 2012
    * [Others]
        * Cleanup, introducing git flow, readme creation
* 10   VI 2012
    * [Languages]
        * Working version of Enriched Lambda Calculus With Low Level Types Interpreter
*  7   VI 2012
    * [Compiler]
        *Translation from Enriched Lambda Calculus to Enriched Lambda Calculus With Low Level Types
* 27    V 2012
    * [Compiler] 
        * Translation from MiniML to Enriched Lambda Calculus
*  4    V 2012
    * [Languages]
        * Working version of Enriched Lambda Calculus interpreter
* 14   IV 2012
    * [Languages] 
        * Working version of MiniML interpreter
*  5  III 2012
    * [Others]
        * The project begins
