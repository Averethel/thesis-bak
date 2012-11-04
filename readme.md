Proof Carrying Code for MiniML
==============================
The idea is to create PCC framework for MiniML - subset of OCaml Light language.

Current status
==============
Implementing MiniML compiler to ZINC bytecode

Currently working on
====================
* Interpreter for enriched lambda calculus (feature/enriched-lambda-interpreter)
* Defunctionalization of MiniML interpreter & ectracting common interface parts to a module (feature/miniml-interpreter-refactor) 

Changelog
=========
* 4 XI 2012
    * [Languages]
        * Added (internal) guards on patterns in MiniML
        * Made case statements internal in MiniML
        * LetRec in MiniML are now less restrictive and are allowing expressions on right hand side (previously only functions were allowed)
        * Added rescue expression to EnrichedLambda
          (it behaves like FatBar in MiniML)
    * [Compiler]
        * Translating patterns with constants to guarded patterns
        * Transforming let and letrec definitions to declarations
          (with only variable patterns allowed)
        * Pattern Matchings translated to FatBars and PatternMatching labdas according to [PJ87]
        * Compilation of pattern matching finished
        * Translation to enriched lambda calculus finished
* 29 X 2012
    * [Languages]
        * Moved prety printing utility functions to PrettyPrint modules from Syntax
* 22 X 2012
    * [Languages]
        * Removed old Enriched Lambda Calculus Code
    * [Compiler]
        * Removing redundant if stateents introduced by pattern matching simplification
        * Added translation to Enriched Lambda Calculus
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
