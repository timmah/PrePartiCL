-- VecXForm.hs
-- T. M. Kelley
-- Dec 20, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module VecXForm where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List (intercalate,intersperse)
import Debug.Trace
import qualified Data.Vector.Unboxed

-- | For an arbitrary type, list its fields and types: in effect generate
-- an instance of Show for the type.
listFieldsAndTypes :: Name -> Q [Dec]
listFieldsAndTypes name = do
  TyConI (DataD _ _ _ [RecC _ fields] _) <- reify name
  let names = map (\(name,_,_)->name) fields
  let types = map (\(_,_,typ) ->typ ) fields
  let showField :: Name -> Type -> Q Exp
      showField name (ConT ty) = [|\x->s ++ " = " ++ show ($(global name) x)
                                    ++ " :: " ++ tyName|] where
                  s = nameBase name
                  tyName = nameBase ty
      showField name t1 = -- trace (show name ++ ":" ++ show t1) $ 
        [|\x->s ++ " = " ++ show ($(global name) x)
           ++ " :: " ++ (tname)|] where
                  s = nameBase name
                  tname = show t1
  let showFields :: Q Exp
      showFields = listE $ zipWith showField names types
  [d|instance Show $(conT name) where
      show x = intercalate ",\n" (map ($ x) $showFields)|]

vecSuffix = "Vectromatic"

{- Given a record type whose fields are instances of Unbox, generate 
 - a record type with fields that are vectors of the unboxed type.
 - Does not accomodate ordinary ctors, multiple ctors, 
 - or type contexts. If any fields are not instances of Unbox, fails
 - with list of those fields.
 -
 - The name of the new type will be the name of the old type with 
 - "Vectromatic" appended
 - To do: capture and propagate the ctor name for the record type.-}
mkUnboxedVector :: Name -> Q [Dec]
mkUnboxedVector name = do
  -- Get the name's type
  TyConI (DataD _ tyN _ [RecC valN fields] _) <- reify name
  let types :: [[Type]]
      types = map (\(_,_,t)->[t]) fields
  let ubName = ''Data.Vector.Unboxed.Unbox
  let vName  = ''Data.Vector.Unboxed.Vector
  -- Check that type of each record is an instance of Unboxed
  instDecs <- sequenceQ $ map (isInstance ubName) types  
  let fails :: [Type]
      fails = map (\(_,[t])->t) $ filter (\(b,t)->not b) $ zip instDecs types
  -- Create new field names, types
  let newTyNm :: Name
      newTyNm = mkName $ (nameBase tyN) ++ vecSuffix
  let appField :: (VarStrictType) -> (VarStrictType)
      appField (n,s,t) =
        (mkName (nameBase n++"V"),
         NotStrict,
         (AppT (ConT . mkName $ "Data.Vector.Unboxed.Vector") t))
  let appFields = map appField
  -- Emit the new data type with Unboxed vector fields
  case (length fails) of
    0 -> return [DataD [] newTyNm [] [RecC newTyNm (appFields fields)] []]
    _ -> fail $ mkErrStr "must be an instance of Unbox" fails

-- | For each record type that is not an instance of Unbox, remind
-- user that this is required.
mkErrStr :: String -> [Type] -> String
mkErrStr rt ts = concat . intersperse "\n" $ tstrs
  where tstrs :: [String]
        tstrs = map (\t->(show t) ++ " " ++ rt) ts

-- End of file
