{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad   (zipWithM)
import qualified Data.Aeson      as A
import qualified Data.Aeson.Key  as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Vector     as V
import qualified Data.Text       as T
import           Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "about.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "programs/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/program.html" programCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            programs <- recentFirst =<< loadAll "programs/*"
            let indexCtx =
                    listField "programs" defaultContext (return programs) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


-- ── Contexts ───────────────────────────────────────────────

-- | A single work entry parsed from YAML frontmatter.
data Work = Work
    { workNumber    :: String
    , workComposer  :: String
    , workTitle     :: String
    , workMovements :: [String]
    }

movementCtx :: Context String
movementCtx = bodyField "movement"

workCtx :: Context Work
workCtx =
    field "number"   (return . workNumber   . itemBody) <>
    field "composer" (return . workComposer . itemBody) <>
    field "title"    (return . workTitle    . itemBody) <>
    listFieldWith "movements" movementCtx
        (traverse makeItem . workMovements . itemBody)

programCtx :: Context String
programCtx = overviewCtx <> worksListCtx <> defaultContext
  where
    overviewCtx = field "overview" $ \item -> do
        mMd <- getMetadataField (itemIdentifier item) "overview"
        case mMd of
            Nothing -> return ""
            Just md -> fmap itemBody . renderPandoc =<< makeItem md

    worksListCtx = listFieldWith "works" workCtx extractWorks

    extractWorks item = do
        meta <- getMetadata (itemIdentifier item)
        let vals = case A.toJSON meta of
                A.Object obj ->
                    case AKM.lookup (AK.fromString "works") obj of
                        Just (A.Array arr) -> V.toList arr
                        _                  -> []
                _ -> []
        zipWithM makeWorkItem [1..] vals

    makeWorkItem :: Int -> A.Value -> Compiler (Item Work)
    makeWorkItem n (A.Object obj) = makeItem Work
        { workNumber    = padNum n
        , workComposer  = getStr  "composer"  obj
        , workTitle     = getStr  "title"     obj
        , workMovements = getStrs "movements" obj
        }
    makeWorkItem _ _ = makeItem (Work "00" "" "" [])

    getStr k m = case AKM.lookup (AK.fromString k) m of
        Just (A.String t) -> T.unpack t
        _                 -> ""

    getStrs k m = case AKM.lookup (AK.fromString k) m of
        Just (A.Array v) -> [T.unpack t | A.String t <- V.toList v]
        _                -> []

    padNum n = if n < (10 :: Int) then "0" <> show n else show n
