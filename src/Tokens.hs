module Tokens where

import Data.List


data SchemeProcedure = AnonFn { abstract :: Scheme }
                     | Fn { fnName :: String }


data ConsCellData = ConsCellData Scheme Scheme

data Scheme = Number Integer
            | String String
            | Char Char
            | Bool Bool
            | ConsCell ConsCellData
            | List ConsCellData
            | Ident String
            | Procedure SchemeProcedure
            | Apply { procedure :: Scheme, args :: [Scheme] }
            | Quote Scheme
            | Void
            | Let [(String, Scheme)] Scheme
            | Def String Scheme
            | DefFn String [String] Scheme Bool
            | AbstractFn String [String] Scheme Bool
            | Cond [Scheme]
            | If Scheme Scheme Scheme
            | And [Scheme]
            | Or [Scheme]


instance Show SchemeProcedure where
  show p = case p of
             AnonFn _ -> ""
             Fn name -> name


instance Show Scheme where
  show scheme = case scheme of
    Number nb -> show nb
    String str -> "\"" ++ str ++ "\""
    Char '\a' -> "#\\alarm"
    Char '\b' -> "#\\backspace"
    Char '\t' -> "#\\tab"
    Char '\n' -> "#\\newline"
    Char '\v' -> "#\\vtab"
    Char '\f' -> "#\\page"
    Char '\r' -> "#\\return"
    Char ' ' -> "#\\space"
    Char c -> "#\\" ++ [c]
    ConsCell (ConsCellData car cdr) -> "(" ++ (show car) ++ " . " ++ (displayCdrCell cdr) ++ ")"
      where
        displayCdrCell (ConsCell (ConsCellData _car Void)) = show _car
        displayCdrCell (ConsCell (ConsCellData _car _cdr)) = (show _car) ++ " " ++ displayCdrCell _cdr
        displayCdrCell _cdr = show _cdr
    List (ConsCellData car cdr) -> "(" ++ (show car) ++ (evalRight cdr) ++ ")"
      where
        evalRight _cdr = case _cdr of
          ConsCell(ConsCellData Void Void) -> ""
          ConsCell(ConsCellData x Void) -> " " ++ (show x)
          ConsCell(ConsCellData x y) -> " " ++ (show x) ++ (evalRight y)
          Void -> ""
          x -> " " ++ (show x)
    Void -> ""
    Ident str -> str
    Def str val -> "(define " ++ str ++ " " ++ show val ++ ")"
    Bool bool -> if bool then "#t" else "#f"
    Procedure p -> "#<procedure" ++ format ++ ">"
      where
        format = if length (show p) == 0
          then ""
          else " " ++ show p
    AbstractFn name _ _ _ -> show $ Procedure $ Fn name
    Quote expr -> "'" ++ show expr
    Apply (Procedure ap) _args -> "(" ++ show ap ++ " " ++ pArgs ++ ")"
      where
        pArgs = (intercalate " " (map show _args))
    If cond _then _else -> "(if " ++ show cond ++ " " ++ show _then ++ " " ++ show _else ++ ")"
    Apply x _ -> show x

