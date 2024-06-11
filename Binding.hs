module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step expr = case expandMacros ctx expr of
    Left err -> Left err
    Right expandedExpr -> Right (simplify step expandedExpr)

expandMacros :: Context -> Lambda -> Either String Lambda
expandMacros ctx (Var x) = Right (Var x)
expandMacros ctx (App e1 e2) = do
                            e1' <- expandMacros ctx e1
                            e2' <- expandMacros ctx e2
                            return (App e1' e2')
expandMacros ctx (Abs x e) = Abs x <$> expandMacros ctx e
expandMacros ctx (Macro name) = case lookup name ctx of
                                Just e -> Right e
                                Nothing -> Left $ "Undefined macro: " ++ name

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
