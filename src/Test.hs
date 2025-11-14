{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import "Glob" System.FilePath.Glob (glob)
import Control.Monad
import Data.Makefile
import Data.Makefile.Parse
import Data.Makefile.Parse.Internal
import Data.Makefile.Render
import Data.Makefile.Render.Internal
import Test.DocTest (doctest)
import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

genAZ :: Gen T.Text
genAZ = T.pack <$> listOf1 (choose ('a', 'z'))

instance Arbitrary Target where
  arbitrary = Target <$> genAZ

instance Arbitrary Dependency where
  arbitrary = Dependency <$> genAZ

instance Arbitrary Command where
  arbitrary = Command <$> genAZ

instance Arbitrary AssignmentType where
  arbitrary =
    elements [minBound..maxBound]

instance Arbitrary Entry where
  arbitrary =
    oneof
      [ Rule <$> arbitrary <*> arbitrary <*> arbitrary
      , Assignment <$> arbitrary <*> genAZ <*> genAZ
      , (OtherLine . T.cons '#') <$> genAZ
      , pure $ OtherLine ""
      ]

instance Arbitrary Makefile where
  arbitrary = Makefile <$> arbitrary

main :: IO ()
main = defaultMain $ testGroup "makefile tests"
    [ testCase "doctest" doc
    , basicMakefileTests
    , elfparseMakefileTests
    , allSyntaxTests
    , testProperty "encode decode" prop_encodeDecode
    , testProperty "encoded does end with newline" prop_encodeNewline
    ]

isId :: Eq a => (a -> a) -> a -> Bool
isId f a = f a == a

-- | We ensure that all encoded entries finish with a new line character
-- (lines/unlines)
prop_encodeNewline :: Entry -> Bool
prop_encodeNewline e =
    let encoded = encodeEntry e
    in TL.length encoded /= 0 && (TL.last encoded == '\n')

prop_encodeDecode :: Makefile -> Bool
prop_encodeDecode =
    isId $
      fromRight
      . parseMakefileContents
      . TL.toStrict
      . encodeMakefile

withMakefileContents :: T.Text -> (Makefile -> IO ()) -> IO ()
withMakefileContents contents a =
  a $ fromRight (parseMakefileContents contents)

withMakefile :: FilePath -> (Makefile -> IO ()) -> IO ()
withMakefile  f a = fromRight <$> parseAsMakefile f >>= a

assertMakefile :: Makefile -> Makefile -> IO ()
assertMakefile m1 m2 =
  unless (m1 == m2)
    $ error $ unwords
        [ "Makefiles mismatch!"
        , "got " <> show m1
        , "and " <> show m2
        ]

assertTargets :: [Target] -> Makefile -> IO ()
assertTargets ts m = mapM_ (`assertTarget` m) ts

assertAssignments :: [(T.Text, T.Text)] -> Makefile -> IO ()
assertAssignments as m = mapM_ (`assertAssignment` m) as

assertAssignment :: (T.Text, T.Text) -> Makefile -> IO ()
assertAssignment (n, v) (Makefile m) = unless (any hasAssignment m) $
    error ("Assignment " ++ show (n, v) ++ " wasn't found in Makefile " ++ show m)
  where hasAssignment (Assignment _ n' v') = n == n' && v == v'
        hasAssignment _                  = False

assertTarget :: Target -> Makefile -> IO ()
assertTarget t (Makefile m) = unless (any hasTarget m) $
    error ("Target " ++ show t ++ " wasn't found in Makefile " ++ show m)
  where hasTarget (Rule t' _ _) = t == t'
        hasTarget _                 = False

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight"

doc :: IO ()
doc = glob "src/**/*.hs" >>= doctest

basicMakefileTests :: TestTree
basicMakefileTests = testGroup "basicMakefileTests"
    [ testCase "Makefile1 targets" $
        withMakefile "test-data/basic/Makefile1" $ \m -> assertTarget "foo" m
    , testCase "Makefile2 targets" $
        withMakefile "test-data/basic/Makefile2" $ \m -> do
            assertTargets [ "all"
                          , "hello"
                          , "main.o"
                          , "factorial.o"
                          , "hello.o"
                          , "clean"] m
            assertAssignment ("CC", "g++") m
    , testCase "Makefile1 read/write" $
        withMakefile "test-data/basic/Makefile1" $ \m -> do
          writeMakefile  "test-data/basic/_Makefile1" m
          withMakefile "test-data/basic/_Makefile1" $ \mm ->
            assertMakefile m mm
    , testCase "Makefile2 read/write" $
        withMakefile "test-data/basic/Makefile2" $ \m -> do
          writeMakefile  "test-data/basic/_Makefile2" m
          withMakefile "test-data/basic/_Makefile2" $ \mm ->
            assertMakefile m mm
    ]

elfparseMakefileTests :: TestTree
elfparseMakefileTests = testCase "elfparseMakefileTests" $
    withMakefile "test-data/elfparse/Makefile" $ \m -> do
        assertTargets [ "default"
                      , "is_perl_recent_enough"
                      , "all"
                      , "clean"
                      , "clean_not_caches"
                      , "${SRCP6}/STD.pmc"
                      , "lex.is_current"
                      , "${STD_BLUE_CACHEDIR}.is_current"
                      , "${STD_RED_CACHEDIR}.is_current"
                      , "IRx1_FromAST2.pm"
                      , "elfblue"
                      , "elfrx"
                      , "nodes.pm"
                      , "rx_prelude.pm"
                      , "rx_prelude_p5.pm"
                      , "std.pm.p5"
                      , "STD_green_run.pm.p5"
                      , "STD_green_run"
                      , "elfdev"
                      , "elfdev1"
                      , "check"
                      , "check_rx_on_re"
                      , "check_std_rx_on_re"
                      , "rerun_std_rx_on_re"
                      , "check_STD_blue"
                      , "does_gimme5_memory_problem_still_exist"
                      , "elfblue_regression_debug"
                      , "have_STD_red_cache"
                      , "have_STD_blue_cache" ] m
        assertAssignments [ ("ELF", "../../elf/elf_h")
                          , ("ELFDIR", "../../elf/elf_h_src")
                          , ("SRCP6", "./pugs_src_perl6")
                          , ("STDPM", "./pugs_src_perl6/STD.pm")
                          , ("TMP", "deleteme")
                          {- ... feeling lazy -} ] m

allSyntaxTests :: TestTree
allSyntaxTests = testGroup "syntax tests"
    [ testCase "simple assignment" $
        withMakefileContents
          "foo = bar"
          (assertAssignments [("foo", "bar")])
    , testCase "simple target" $
        withMakefileContents "foo: bar" (assertTargets ["foo"])
    , testCase "simple assign + target" $
        withMakefileContents
          (T.pack $ unlines
            [ "var="
            , "foo: bar"
            ]
          )
          (assertMakefile
            Makefile
              { entries =
                  [ Assignment RecursiveAssign "var" ""
                  , Rule "foo" ["bar"] []
                  ]
              }
            )
    , testCase "empty lines" $
        let m = Makefile { entries = [OtherLine "", OtherLine ""] }
        in assertMakefile m $
            fromRight
            . parseAll makefile
            . TL.toStrict
            $ encodeMakefile m
    , testCase "assign with spaces" $
        withMakefileContents
          (T.pack $ unlines
            [ "var=foo bar" ]
          )
          (assertAssignments [("var", "foo bar")])
    , testCase "assign escaped" $
        withMakefileContents
          (T.pack $ unlines
            [ "var=foo bar\\"
            , "baz"
            ]
          )
          (assertAssignments [("var", "foo bar baz")])
    , testCase "assign spaces escaped" $
        withMakefileContents
          (T.pack $ unlines
            [ "var=foo bar    \\"
            , "baz"
            ]
          )
          (assertAssignments [("var", "foo bar baz")])
    , testCase "assign space newline" $
        withMakefileContents
          (T.pack $ unlines
            [ "var=foo bar\\"
            , "   baz"
            ]
          )
          (assertAssignments [("var", "foo bar baz")])
    , testCase "assign spaces escaped tab" $
        withMakefileContents
          (T.pack $ unlines
            [ "var=foo bar    \\"
            , "\tbaz"
            ]
          )
          (assertAssignments [("var", "foo bar baz")])
    , testCase "assign spaces tab escaped tab" $
        withMakefileContents
          (T.pack $ unlines
            [ "var=foo bar  \t  \\"
            , "  \t  baz"
            ]
          )
          (assertAssignments [("var", "foo bar baz")])
    , testCase "assign multiline" $
        withMakefileContents
          (T.pack $ unlines
            [ "SUBDIRS=anna bspt cacheprof \\"
            , "        compress compress2 fem"
            ]
          )
          (assertAssignments
            [("SUBDIRS", "anna bspt cacheprof compress compress2 fem")])
    , testCase "assign multiline 2" $
        withMakefileContents
          (T.pack $ unlines
            [ "foo: anna bspt cacheprof \\"
            , "  compress compress2 fem"
            ]
          )
          (assertMakefile
            Makefile
              { entries =
                  [ Rule
                      "foo"
                      [ "anna"
                      , "bspt"
                      , "cacheprof"
                      , "compress"
                      , "compress2"
                      , "fem"
                      ] []
                  ]
              }
            )
    -- Courtesy of quickcheck
    , testCase "target stuff" $
        withMakefileContents
          (T.pack $ unlines
            [ ""
            , "# some comment"
            , "foo::=bar"
            ]
          )
          (assertMakefile
            Makefile
              { entries =
                  [ OtherLine ""
                  , OtherLine "# some comment"
                  , Assignment SimplePosixAssign "foo" "bar"
                  ]
              }
            )
    , recipeTests
    ]

recipeTests :: TestTree
recipeTests = testGroup "recipe tests"
  [ testCase "simple recipe" $
      withMakefileContents
        (T.pack $ unlines
            [ "foo:"
            , "\techo foo"
            ]
        )
        (assertMakefile
          Makefile
            { entries =
                [ Rule "foo" [] ["echo foo"]
                ]
            }
          )
  , testCase "do not eat shell comments" $
      withMakefileContents
        (T.pack $ unlines
            [ "foo:"
            , "\t#hello"
            , "\t#world"
            ]
        )
        (assertMakefile
          Makefile
            { entries =
                [ Rule "foo" []
                    ["#hello"
                    ,"#world"]
                ]
            }
          )
  , testCase "multi line" $
      withMakefileContents
        (T.pack $ unlines
            [ "foo:"
            , "\tcd dir/ && \\"
            , "ls"
            ]
        )
        (assertMakefile
          Makefile
            { entries =
                [ Rule "foo" [] ["cd dir/ && \\\nls"]
                ]
            }
          )
  , testCase "multi line tab strip" $
      withMakefileContents
        (T.pack $ unlines
            [ "foo:"
            , "\tcd dir/ && \\"
            , "\tls"
            ]
        )
        (assertMakefile
          Makefile
            { entries =
                [ Rule "foo" [] ["cd dir/ && \\\nls"]
                ]
            }
          )
  ]
