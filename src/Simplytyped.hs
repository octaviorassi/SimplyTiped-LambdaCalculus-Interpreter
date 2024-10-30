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
conversion' idxs (LVar x)         = case M.lookup x idxs of
                                    Just i  -> Bound i
                                    Nothing -> Free (Global x)

conversion' idxs (LAbs x ty t)    = let idxs'   = M.map succ idxs
                                      idxs''  = M.insert x 0 idxs'
                                  in  Lam ty (conversion' idxs'' t)

conversion' idxs (LApp t1 t2)     = t1' :@: t2'
                                  where
                                    t1' = conversion' idxs t1
                                    t2' = conversion' idxs t2

conversion' idxs (LLet x t1 t2)   = let
                                    idxs' =  M.map succ idxs
                                    idxs'' = M.insert x 0 idxs' 

                                    t1' = conversion' idxs    t1   -- no usamos idxs'' porque idxs'' tiene a la lambda falsa de t2
                                    t2' = conversion' idxs''  t2 
                                  in
                                    Let t1' t2'

conversion' idxs LZero            = Zero
conversion' idxs (LSuc t)         = Suc (conversion' idxs t)
conversion' idxs (LRec t1 t2 t3)  = Rec t1' t2' t3'
                                    where
                                      t1' = conversion' idxs t1
                                      t2' = conversion' idxs t2
                                      t3' = conversion' idxs t3

conversion' idxs LNil             = Nil
conversion' idxs (LCons x xs)     = Cons x' xs'
                                    where x'  = conversion' idxs x
                                          xs' = conversion' idxs xs

conversion' idxs (LRecL t1 t2 t3)  = Rec t1' t2' t3'
                                    where
                                      t1' = conversion' idxs t1
                                      t2' = conversion' idxs t2
                                      t3' = conversion' idxs t3

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
sub i t Zero                  = Zero
sub i t (Suc t')              = Suc (sub i t t')
sub i t (Rec t1 t2 t3)        = Rec t1' t2' t3'
                                where
                                  t1' = sub i t t1
                                  t2' = sub i t t2
                                  t3' = sub i t t3

sub i t Nil                   = Nil

sub i t (Cons x xs)           = Cons x' xs'
                                where x' = sub i t x
                                      xs' = sub i t xs 

sub i t (RecL t1 t2 t3)       = RecL t1' t2' t3'
                                where
                                  t1' = sub i t t1
                                  t2' = sub i t t2
                                  t3' = sub i t t3


-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f)       = Lam t f
quote (VNum vn)        = numvalToTerm vn
quote (VList vl)       = listvalToTerm vl

numvalToTerm :: NumVal -> Term
numvalToTerm NZero     = Zero
numvalToTerm (NSuc nv) = Suc (numvalToTerm nv)

listvalToTerm :: ListVal -> Term
listvalToTerm VNil = Nil
listvalToTerm (VCons nv lv) = Cons nv' lv'
                              where nv' = numvalToTerm nv
                                    lv' = listvalToTerm lv

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

eval ne Zero        = VNum NZero
eval ne (Suc t)     = VNum (NSuc t')
                      where (VNum t') = eval ne t

eval ne (Rec t1 t2 t3) = case eval ne t3 of
                          VNum NZero     -> eval ne t1
                          VNum (NSuc nv) -> eval ne ((t2 :@: (Rec t1 t2 nv')) :@: nv')
                                            where nv' = numvalToTerm nv
                        
eval ne Nil         = VList VNil
eval ne (Cons x xs) = VList (VCons x' xs')
                      where (VNum x')   = eval ne x
                            (VList xs') = eval ne xs
eval ne (RecL t1 t2 t3) = case eval ne t3 of 
                          (VList VNil)          ->  eval ne t1
                          (VList (VCons nv lv)) ->  eval ne (t2 :@: nv' :@: lv' :@: (RecL t1 t2 lv'))
                                                    where nv' = numvalToTerm nv
                                                          lv' = listvalToTerm lv


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
-- ? agregar un error de cantidad de argumentos de la funcion?
infer' :: Context -> NameEnv Value Type -> Term -> Either String   Type
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

infer' c e Zero     = ret NatT
infer' c e (Suc t)  = infer' c e t >>= \tt -> case tt of
                                                NatT -> ret NatT
                                                _    -> matchError NatT tt
infer' c e (Rec t1 t2 t3) = infer' c e t3 >>= \tt3 -> case tt3 of
                                                        NatT -> infer' c e t1 >>= \tt1 -> infer' c e t2 >>= \tt2 -> if tt2 == FunT tt1 (FunT NatT tt1)
                                                                                                                  then ret tt1
                                                                                                                  else matchError (FunT tt1 (FunT NatT tt1)) tt2 
                                                        _    -> matchError NatT tt3

infer' c e Nil            = ret ListT
infer' c e (Cons t1 t2)   = infer' c e t1 >>= \tt1 ->  case tt1 of 
                                                          NatT -> infer' c e t2 >>= \tt2 -> if tt2 == ListT
                                                                                            then ret ListT
                                                                                            else matchError ListT tt2
                                                          _    -> matchError NatT tt1
                                                          
infer' c e (RecL t1 t2 t3) = infer' c e t3 >>= \tt3 -> infer' c e t1 >>= \tt1 -> infer' c e t2 >>= \tt2 ->
                              case tt3 of 
                              ListT ->  if tt2 == expectedType
                                        then ret tt1
                                        else matchError expectedType tt2
                                        where expectedType = FunT NatT (FunT ListT (FunT tt1 tt1))
                              _  -> matchError ListT tt3