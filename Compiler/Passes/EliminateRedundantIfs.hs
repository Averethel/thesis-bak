module Compiler.Passes.EliminateRedundantIfs (eliminate_redundant_ifs) where
  import Languages.MiniML.Syntax

  -- The purpose here is to elminate lot of rendundant if statements introduced
  -- by unfolding pattern matching to if tests

  eliminiate_redundant_ifs_expression :: Expr -> Expr
  eliminiate_redundant_ifs_expression (E_Apply e1 e2)                  =
    let e1' = eliminiate_redundant_ifs_expression e1
        e2' = eliminiate_redundant_ifs_expression e2
    in  E_Apply e1' e2'
  eliminiate_redundant_ifs_expression (E_Cons e1 e2)                   =
    let e1' = eliminiate_redundant_ifs_expression e1
        e2' = eliminiate_redundant_ifs_expression e2
    in  E_Cons e1' e2'
  eliminiate_redundant_ifs_expression (E_Tuple es)                     =
    E_Tuple $ map eliminiate_redundant_ifs_expression es
  eliminiate_redundant_ifs_expression (E_And e1 e2)                    =
    let e1' = eliminiate_redundant_ifs_expression e1
        e2' = eliminiate_redundant_ifs_expression e2
    in  E_And e1' e2'
  eliminiate_redundant_ifs_expression (E_Or e1 e2)                     =
    let e1' = eliminiate_redundant_ifs_expression e1
        e2' = eliminiate_redundant_ifs_expression e2
    in  E_Or e1' e2'
  eliminiate_redundant_ifs_expression (E_ITE (E_Const C_True) e2 e3)   =
    eliminiate_redundant_ifs_expression e2
  eliminiate_redundant_ifs_expression (E_ITE (E_Const C_False) e2 e3)  =
    eliminiate_redundant_ifs_expression e3
  eliminiate_redundant_ifs_expression (E_ITE 
                                (E_Let _ (E_Const C_True)) e2 e3)      =
    eliminiate_redundant_ifs_expression e2
  eliminiate_redundant_ifs_expression (E_ITE 
                                (E_Let _ (E_Const C_False)) e2 e3)     =
    eliminiate_redundant_ifs_expression e3
  eliminiate_redundant_ifs_expression (E_ITE 
                                (E_LetRec _ (E_Const C_True)) e2 e3)   =
    eliminiate_redundant_ifs_expression e2
  eliminiate_redundant_ifs_expression (E_ITE 
                                (E_LetRec _ (E_Const C_False)) e2 e3)  =
    eliminiate_redundant_ifs_expression e3
  eliminiate_redundant_ifs_expression (E_ITE e1 
                                (E_Const C_True) (E_Const C_False))    =
    eliminiate_redundant_ifs_expression e1
  eliminiate_redundant_ifs_expression (E_ITE e1 e2 e3)                 =
    let e1' = eliminiate_redundant_ifs_expression e1
        e2' = eliminiate_redundant_ifs_expression e2
        e3' = eliminiate_redundant_ifs_expression e3
    in  E_ITE e1' e2' e3'
  eliminiate_redundant_ifs_expression (E_Case e bs)                    =
    let e'  = eliminiate_redundant_ifs_expression e
        bs' = map eliminiate_redundant_ifs_binding bs
    in  E_Case e' bs'
  eliminiate_redundant_ifs_expression (E_Seq e1 e2)                    =
    let e1' = eliminiate_redundant_ifs_expression e1
        e2' = eliminiate_redundant_ifs_expression e2
    in  E_Seq e1' e2'
  eliminiate_redundant_ifs_expression (E_Function bs)                  =
    E_Function $ map eliminiate_redundant_ifs_binding bs
  eliminiate_redundant_ifs_expression (E_Let bs e)                     =
    let e'  = eliminiate_redundant_ifs_expression e
        bs' = map eliminiate_redundant_ifs_binding bs
    in  E_Let bs' e'
  eliminiate_redundant_ifs_expression (E_LetRec lrbs e)                =
    let e'    = eliminiate_redundant_ifs_expression e
        lrbs' =  map (\(n, bs) -> (n, map eliminiate_redundant_ifs_binding bs)) lrbs
    in E_LetRec lrbs' e'
  eliminiate_redundant_ifs_expression e                                = e

  eliminiate_redundant_ifs_binding :: Binding -> Binding
  eliminiate_redundant_ifs_binding (p, e) = 
    (p, eliminiate_redundant_ifs_expression e)

  eliminiate_redundant_ifs_definition :: Definition -> Definition
  eliminiate_redundant_ifs_definition (D_Let bs)      = 
    D_Let $ map eliminiate_redundant_ifs_binding bs
  eliminiate_redundant_ifs_definition (D_LetRec lrbs) =
    D_LetRec $ map (\(n, bs) -> (n, map eliminiate_redundant_ifs_binding bs)) lrbs

  eliminiate_redundant_ifs_instruction :: Instruction -> Instruction
  eliminiate_redundant_ifs_instruction (IEX ex) = 
    IEX $ eliminiate_redundant_ifs_expression ex
  eliminiate_redundant_ifs_instruction (IDF df) =
    IDF $ eliminiate_redundant_ifs_definition df

  eliminate_redundant_ifs :: Program -> Program
  eliminate_redundant_ifs = map eliminiate_redundant_ifs_instruction
