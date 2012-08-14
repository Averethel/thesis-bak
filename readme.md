Proof Carrying Code for MiniML
==============================
The idea is to create PCC framework for MiniML - subset of OCaml Light language.

Current status
==============
Implementing MiniML compiler to simple asembly language.

Currently working on
====================
* Assembly language (see feature/languages/assembly)
* Code refactoring  (see feature/refactoring)
    1. Translation improvemets
        * ~~enforce unique functions / variable names~~
        * ~~update MiniML -> Enriched Lambda Calculus translation to handle definitions~~
        * update Enriched Lambda Calculus -> Enriched Lambda Calculus With Low Level Types translation to handle definitions
        * move all definitions to the beginning of the program
    2. Other improvements
        * state monad in MiniML interpreter
        * use record for state in Enriched Lambda Calculus interpreter
        * ~~some renaming for consistency~~
        * ~~add definitions to Enriched Lambda Calculus~~
        * add definitions to Enriched Lambda Calculus With Low Level Types
        * ~~allow mutually recursive letrec in Enriched Lambda Calculus~~
        * allow mutually recursive letrec in Enriched Lambda Calculus With Low Level Types

Changelog
=========
* 15 VIII 2012 - [Compiler]  Finished transaltion from MiniML to Enriched Lambda Calculus
* 11 VIII 2012 - [Compiler]  Enforcing unique variable naming
* 11 VIII 2012 - [Languages] Mutually recursive letrec in Enriched Lambda Calculus
*  7 VIII 2012 - [Languages] Added definitions to Enriched Lambda Calculus
*  6 VIII 2012 - [Others]    Cleanup, introducing git flow, readme creation
* 10   VI 2012 - [Languages] Working version of Enriched Lambda Calculus With Low Level Types Interpreter
*  7   VI 2012 - [Compiler]  Translation from Enriched Lambda Calculus to Enriched Lambda Calculus With Low Level Types
* 27    V 2012 - [Compiler]  Translation from MiniML to Enriched Lambda Calculus
*  4    V 2012 - [Languages] Working version of Enriched Lambda Calculus interpreter
* 14   IV 2012 - [Languages] Working version of MiniML interpreter
*  5  III 2012 - [Others]    The project begins
