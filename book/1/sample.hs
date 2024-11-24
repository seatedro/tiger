type Id = String

data BinOp = Plus | Minus | Times | Div
  deriving (Show)

data Exp
  = IdExp Id
  | NumExp Int
  | OpExp Exp BinOp Exp
  | EseqExp Stm Exp
  deriving (Show)

data Stm
  = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]
  deriving (Show)

prog =
  CompoundStm
    ( AssignStm
        "a"
        (OpExp (NumExp 5) Plus (NumExp 3))
    )
    ( CompoundStm
        ( AssignStm
            "b"
            ( EseqExp
                (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                (OpExp (NumExp 10) Times (IdExp "a"))
            )
        )
        (PrintStm [IdExp "b"])
    )

maxargs :: Stm -> Int
maxargs stm = case stm of
  PrintStm exps -> max (maximum (0 : map maxargsExp exps)) (length exps)
  AssignStm _ e -> maxargsExp e
  CompoundStm s1 s2 -> max (maxargs s1) (maxargs s2)

maxargsExp :: Exp -> Int
maxargsExp exp = case exp of
  EseqExp s e -> max (maxargs s) (maxargsExp e)
  OpExp e1 _ e2 -> max (maxargsExp e1) (maxargsExp e2)
  NumExp _ -> 0
  IdExp _ -> 0

main = print (maxargs prog)
