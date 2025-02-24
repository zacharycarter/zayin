{-# LANGUAGE OverloadedStrings #-}

module Zayin.CExport (generateC) where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Zayin.Codegen (CDecl (..), CExpr (..), CStmt (..), CType (..))

class ToC a where
  toC :: a -> T.Text

instance ToC CExpr where
  toC expr = case expr of
    EVar name -> name
    ELitInt n -> T.pack (show n)
    ELitStr s -> "\"" <> s <> "\""
    ELitBool True -> "1"
    ELitBool False -> "0"
    EBinOp op left right -> "(" <> toC left <> ")" <> op <> "(" <> toC right <> ")"
    EPreUnOp op ex -> op <> "(" <> toC ex <> ")"
    EArrow expr attr -> "(" <> toC expr <> ")->" <> attr
    ECast ex typ -> "(" <> toCType typ <> ")(" <> toC ex <> ")"
    EMacroCall name args -> name <> "(" <> T.intercalate "," (map toC args) <> ")"
    ENull -> "NULL"

instance ToC CStmt where
  toC stmt = case stmt of
    SExpr expr -> toC expr <> ";\n"
    SDecl decl -> toC decl
    SIf cond ift iff -> "if (" <> toC cond <> ")" <> toC ift <> " else " <> toC iff
    SBlock stmts -> "{\n" <> T.concat (map toC stmts) <> "}\n"

instance ToC CDecl where
  toC decl = case decl of
    DStruct {dName = name, dMembers = members} ->
      "struct "
        <> name
        <> " {\n"
        <> T.concat [toCTypeWithName typ n <> ";\n" | (n, typ) <- members]
        <> "};\n"
    DFunProto {dName = name, dRetType = ret, dParams = params, dNoreturn = noret} ->
      toCType ret
        <> " "
        <> name
        <> "("
        <> T.intercalate "," [toCTypeWithName t n | (n, t) <- params]
        <> ")"
        <> (if noret then " __attribute__((noreturn))" else "")
        <> ";\n"
    DFun {dName = name, dRetType = ret, dParams = params, dBody = body} ->
      toCType ret
        <> " "
        <> name
        <> "("
        <> T.intercalate "," [toCTypeWithName t n | (n, t) <- params]
        <> ") {\n"
        <> T.concat (map toC body)
        <> "}\n"
    DVar {dName = name, dType = typ, dInit = minit} ->
      toCTypeWithName typ name
        <> maybe "" (\init -> " = " <> toC init) minit
        <> ";\n"

toCType :: CType -> T.Text
toCType typ = case typ of
  TVoid -> "void"
  TPtr t -> toCType t <> "*"
  TStruct name -> "struct " <> name

toCTypeWithName :: CType -> T.Text -> T.Text
toCTypeWithName typ name = case typ of
  TVoid -> "void " <> name
  TPtr t -> toCTypeWithName t ("*" <> name)
  TStruct sname -> "struct " <> sname <> " " <> name

-- Filter out assignments that copy from input_env in the main lambda.
filterMainLambdaAssignments :: [CStmt] -> [CStmt]
filterMainLambdaAssignments = filter (not . isInputEnvCopy)
  where
    isInputEnvCopy (SExpr (EBinOp "=" _ rhs)) =
      case rhs of
        EArrow (EVar v) _ -> v == "env"
        _ -> False
    isInputEnvCopy _ = False

generateC :: [CStmt] -> [CDecl] -> [CDecl] -> T.Text
generateC rootStmts protos decls =
  T.concat
    [ T.unlines (map toC protos),
      T.unlines (map toC decls),
      toC mainLambda
    ]
  where
    finalStmts = rootStmts ++ [SExpr (EMacroCall "__builtin_unreachable" [])]
    mainLambda =
      DFun
        { dName = "main_lambda",
          dRetType = TVoid,
          dParams =
            [ ("input_obj", TPtr (TStruct "obj")),
              ("env", TPtr (TStruct "env_obj"))
            ],
          dBody = filterMainLambdaAssignments finalStmts
        }
