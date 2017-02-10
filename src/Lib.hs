{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, QuasiQuotes #-}

module Lib
    -- ( someFunc
    -- )
    where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.Meta as LHM

annotations :: QuasiQuoter
annotations = QuasiQuoter {
      quoteExp = \_ -> fail "annotations: called in an expression context."
    , quotePat = \_ -> fail "annotations: called in an pattern context."
    , quoteType = \_ -> fail "annotations: called in an type context."
    , quoteDec = deriveAnnotation
    }


-- | c is annotated as a
class Annotated c a where
  ann :: b -> c -> a b
  unAnn :: a b -> (b, c)

  viewC :: a b -> c
  viewC = snd . unAnn

  getAnn :: a b -> b

  -- getAnn should not be in here, but ghc complains
  -- when I provide a default implimentation
  {-# MINIMAL ann, unAnn, getAnn #-}

data AnnError = SourceError String
              | BugInDeriveAnnotation String
  deriving(Show)

showNormal :: String -> Q [Dec]
showNormal src =
  case LHM.parseDecs src of
    (Left e) -> fail e
    (Right ds) -> return ds

deriveAnnotation :: String -> Q [Dec]
deriveAnnotation src =
  case LHM.parseDecs src of
    (Left e) -> fail e
    (Right ds) ->
        concat <$> sequence (map (\d -> do
                annDec <- toAnnDataDec d
                return [annDec, d]) ds)

toAnnDataDec :: Dec -> Q Dec
toAnnDataDec (DataD ctxt name tyvars constructors derives) = do
  tv <- newName "a"
  let tyvars' = PlainTV tv : tyvars
      name' = mkTag name
  constructors' <- mapM (toAnnBranch name tv) constructors
  return $ DataD ctxt name' tyvars' constructors' derives
toAnnDataDec _ =
  fail "toAnnDataDev: called on non-data declaration"


toAnnBranch :: Name -> Name -> Con -> Q Con
toAnnBranch dataTag tv branch =
  let dataTag' = mkTag dataTag

      appendRecur :: Type -> Type
      appendRecur (AppT t1 t2) = AppT (appendRecur t1) t2
      appendRecur (ConT n) | n == dataTag = AppT (ConT dataTag') (VarT tv)
      appendRecur ty = ty
  in case branch of
        (NormalC tag ts) ->
            return $ NormalC (mkTag tag) ((NotStrict,VarT tv):ts')
                where ts' = map (\(s, ty) -> (s, appendRecur ty)) ts
        -- A record called `Foo` gets the function `unAnnFoo` appended
        -- to all branches
        (RecC tag ts) ->
            let annFun = mkName ("unAnn" ++ nameBase dataTag)
                ts' = map (\(f, s, ty) -> (f, s, appendRecur ty)) ts
             in return $ RecC (mkTag tag) ((annFun, NotStrict,VarT tv):ts')

        -- Not even haskell-source-meta handles these cases, so I'mma
        -- leave them unimplimented for now. Eventually I should get
        -- around to filing a bug report and PR.
        -- TODO(ethan)
        (InfixC ty1 tag ty2) -> fail "toAnnBranch: InfixC unimpliemnted"
        (ForallC tvs cxt con) -> fail "toAnnBranch: ForallC unimpliemnted"

mkTag :: Name -> Name
mkTag tag = mkName (nameBase tag ++ "Ann")
