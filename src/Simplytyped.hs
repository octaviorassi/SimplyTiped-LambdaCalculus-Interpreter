module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common
import           Data.Map.Strict         as M

-----------------------
-- conversion
-----------------------

type BoundedVars = M.Map String Int

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' M.empty

conversion' :: BoundedVars -> LamTerm -> Term
conversion' idxs (LVar x)       = case M.lookup x idxs of
                                    Just i  -> Bound i
                                    Nothing -> Free (Global x)

conversion' idxs (LAbs x ty t)  = let idxs'   = M.map succ idxs
                                      idxs''  = M.insert x 0 idxs'
                                  in  Lam ty (conversion' idxs'' t)

conversion' idxs (LApp t1 t2)   = t1' :@: t2'
                                  where
                                    t1' = conversion' idxs t1
                                    t2' = conversion' idxs t2

conversion' idxs (LLet x t1 t2) = let
                                    idxs' =  M.map succ idxs
                                    idxs'' = M.insert x 0 idxs' 

                                    t1' = conversion' idxs    t1   -- no usamos idxs'' porque idxs'' tiene a la lambda falsa de t2
                                    t2' = conversion' idxs''  t2 
                                  in
                                    Let t1' t2'


----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
-- el Int indica bajo cuantas abstracciones estamos sustituyendo; en principio
-- siempre lo llamaremos con 0, y a medida que nos metemos dentro de abstracciones
-- este indice va aumentando para poder llevar registro de como interpretar las variables.

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let t1 t2)           = Let (sub i t t1) (sub (i + 1) t t2)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f

-- evalúa un término en un entorno dado
-- NameEnv Value Type asigna a cada variable una tupla con su valor y su tipo.
eval :: NameEnv Value Type -> Term -> Value
eval ne (Bound j)   = error "bounded variables shall not be evaluated"

eval ne (Free x)    = case Prelude.lookup x ne of
                      Nothing -> error "free variable not found in name environment"
                      Just (v, _)  -> v

eval ne (Lam ty t)  = VLam ty t

eval ne (t1 :@: t2) = let
                        (Lam _ f) = quote $ eval ne t1
                        t2' = eval ne t2
                        t1sub = sub 0 (quote t2') f
                      in
                        eval ne t1sub

eval ne (Let t1 t2) = let
                        t1' = quote $ eval ne t1
                      in
                        eval ne (sub 0 t1' t2)




----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case Prelude.lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let t1 t2) = infer' c e t1 >>= \tt1 -> infer' (tt1 : c) e t2 >>= \tt2 -> ret tt2


