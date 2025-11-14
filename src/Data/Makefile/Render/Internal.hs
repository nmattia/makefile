{-# LANGUAGE OverloadedStrings #-}

module Data.Makefile.Render.Internal where
import           Data.Makefile
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy.Builder

writeMakefile :: FilePath -> Makefile -> IO ()
writeMakefile f = TL.writeFile f . encodeMakefile

encodeMakefile :: Makefile -> TL.Text
encodeMakefile = toLazyText . renderMakefile

renderMakefile :: Makefile -> Builder
renderMakefile (Makefile es ) =
    mconcat $ renderEntry <$> es

encodeEntry :: Entry -> TL.Text
encodeEntry = toLazyText . renderEntry

renderEntry :: Entry -> Builder
renderEntry = (<> singleton '\n') . renderEntry'
  where
    -- renderEntry' does /not/ append a new line
    renderEntry' (Assignment RecursiveAssign key value ) =
      fromText key <> singleton '=' <> fromText value
    renderEntry' (Assignment SimpleAssign key value ) =
      fromText key <> fromText ":=" <> fromText value
    renderEntry' (Assignment SimplePosixAssign key value ) =
      fromText key <> fromText "::=" <> fromText value
    renderEntry' (Assignment ConditionalAssign key value ) =
      fromText key <> fromText "?=" <> fromText value
    renderEntry' (Assignment ShellAssign key value ) =
      fromText key <> fromText "!=" <> fromText value
    renderEntry' (Assignment AppendAssign key value ) =
      fromText key <> fromText "+=" <> fromText value
    renderEntry' (Rule (Target t) ds cs) =
        fromText t <> singleton ':' <> deps'
          <> cmds
      where
        deps = (\dep -> singleton ' ' <> renderDep dep) <$> ds
        deps' = case deps of { [] -> mempty; xs -> mconcat xs }
        cmds =
          case renderCmd <$> cs of
            [] -> mempty
            xs -> singleton '\n' <> intercalate "\n" xs

    renderEntry' (OtherLine t) = fromText t

intercalate :: Monoid a => a -> [a] -> a
intercalate i (x:y:xs) = x <> i <> intercalate i (y:xs)
intercalate _ [x] = x
intercalate _ [] = mempty

renderDep :: Dependency -> Builder
renderDep (Dependency dep ) = fromText dep

renderCmd :: Command -> Builder
renderCmd (Command cmd) = singleton '\t' <> fromText cmd
