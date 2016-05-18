{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Data.Proxy
import qualified Data.Text as T
import Data.Text.Read(decimal)
import Network.Routing
import Criterion.Main

p0  :: Proxy  "0"; p0  = Proxy
p1  :: Proxy  "1"; p1  = Proxy
p2  :: Proxy  "2"; p2  = Proxy
p3  :: Proxy  "3"; p3  = Proxy
p4  :: Proxy  "4"; p4  = Proxy
p5  :: Proxy  "5"; p5  = Proxy
p6  :: Proxy  "6"; p6  = Proxy
p7  :: Proxy  "7"; p7  = Proxy
p8  :: Proxy  "8"; p8  = Proxy
p9  :: Proxy  "9"; p9  = Proxy
p10 :: Proxy "10"; p10 = Proxy
p11 :: Proxy "11"; p11 = Proxy
p12 :: Proxy "12"; p12 = Proxy
p13 :: Proxy "13"; p13 = Proxy
p14 :: Proxy "14"; p14 = Proxy
p15 :: Proxy "15"; p15 = Proxy
p16 :: Proxy "16"; p16 = Proxy
p17 :: Proxy "17"; p17 = Proxy
p18 :: Proxy "18"; p18 = Proxy
p19 :: Proxy "19"; p19 = Proxy

readInt :: T.Text -> Maybe Int
readInt t = case decimal t of
    Right (i, "") -> Just i
    _ -> Nothing

largePath :: Path '[] Maybe Int
largePath = root $ exact "foo" $
    r p0 $ r p1 $ r p2 $ r p3 $ r p4 $
    r p5 $ r p6 $ r p7 $ r p8 $ r p9 $
    r p10 $ r p11 $ r p12 $ r p13 $ r p14 $
    r p15 $ r p16 $ r p17 $ r p18 $ r p19 $
    action Nothing $ \d -> Just $
        get p0 d + get p1 d + get p2 d + get p3 d + get p4 d +
        get p5 d + get p6 d + get p7 d + get p8 d + get p9 d +
        get p10 d + get p11 d + get p12 d + get p13 d + get p14 d +
        get p15 d + get p16 d + get p17 d + get p18 d + get p19 d
  where
    r p = fetch p readInt

largePathRoute :: Router '[] Maybe Int
largePathRoute = largePath +| empty

testLargePath :: [T.Text]
testLargePath = "foo" : map (T.pack . show) [0..19::Int]

benchLargePath :: [T.Text] -> Maybe Int
benchLargePath = execute largePathRoute "GET"

testRoute :: Router '[] Maybe T.Text
testRoute =
    hello +| param +| (foldr (\i r -> deep i +| r) (after +| empty) [0..100])
  where
    hello = exact "echo" $ exact "hello-world" $ action (Just "GET") $ \_ -> Just "Hello World"

    pS = Proxy :: Proxy "S"
    pI = Proxy :: Proxy "I"
    param = exact "echo" $ exact "plain" $ fetch pS Just $ fetch pI readInt $ action (Just "GET") $ \d ->
        Just $ T.replicate (get pI d) (get pS d)

    deep i = exact "deep" $ exact "foo" $ exact "bar" $ exact "baz" $ exact (T.pack $ show i) $ action (Just "GET") $ \_ ->
        Just (T.pack $ show (i :: Int))

    after = exact "after" $ action (Just "GET") $ \_ -> Just "after"

benchApiaryBenchmark :: [T.Text] -> Maybe T.Text
benchApiaryBenchmark = execute testRoute "GET"

main :: IO ()
main = do
    unless (benchLargePath testLargePath == Just 190) $ fail "largePath: result not matched"

    let hello = ["echo", "hello-world"]
        param = ["echo", "plain", "foo", "12"]
        deep  = ["deep", "foo", "bar", "baz", "100"]
        after = ["after"]
    unless (benchApiaryBenchmark hello == Just "Hello World") $ fail "hello: result not matched"
    unless (benchApiaryBenchmark param == Just (T.replicate 12 "foo")) $ fail "param: result not matched"
    unless (benchApiaryBenchmark deep  == Just "100") $ fail "deep: result not matched"
    unless (benchApiaryBenchmark after == Just "after") $ fail "after: result not matched"

    defaultMain
        [ bench "largePath" $ nf benchLargePath testLargePath
        , bgroup "apiary"
            [ bench "hello" $ nf benchApiaryBenchmark hello
            , bench "param" $ nf benchApiaryBenchmark param
            , bench "deep"  $ nf benchApiaryBenchmark deep
            , bench "after" $ nf benchApiaryBenchmark after
            ]
        ]
