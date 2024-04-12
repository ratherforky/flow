import qualified Control.Monad as Monad
import qualified Flow
import qualified System.Exit as Exit
import qualified Test.HUnit as Test
import qualified Criterion.Main as Cri

import qualified FlowINLINE
import qualified FlowNOINLINE


main :: IO ()
main = do
  benchmark
  -- counts <-
  --   Test.runTestTT $
  --     Test.TestList
  --       [ True Test.~?= True,
  --         (3 Flow.|> succ Flow.|> recip Flow.|> negate) Test.~?= (-0.25 :: Double),
  --         (negate Flow.<| recip Flow.<| succ Flow.<| 3) Test.~?= (-0.25 :: Double),
  --         fmap (Flow.apply 2) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
  --         fmap (2 Flow.|>) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
  --         fmap (2 Flow.|>) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
  --         fmap (Flow.<| 2) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
  --         fmap (Flow.apply 3 . Flow.compose succ) [recip, negate]
  --           Test.~?= [0.25, -4 :: Double],
  --         (succ Flow..> recip Flow..> negate) 3 Test.~?= (-0.25 :: Double),
  --         (negate Flow.<. recip Flow.<. succ) 3 Test.~?= (-0.25 :: Double),
  --         fmap ((\f -> f 3) . (succ Flow..>)) [recip, negate]
  --           Test.~?= [0.25, -4 :: Double],
  --         fmap ((\f -> f 3) . (succ Flow..>)) [recip, negate]
  --           Test.~?= [0.25, -4 :: Double],
  --         fmap ((\f -> f 3) . (Flow.<. succ)) [recip, negate]
  --           Test.~?= [0.25, -4 :: Double],
  --         (3 Flow.!> succ Flow.!> recip Flow.!> negate) Test.~?= (-0.25 :: Double),
  --         (undefined Flow.|> const True) Test.~?= True,
  --         (negate Flow.<! recip Flow.<! succ Flow.<! 3) Test.~?= (-0.25 :: Double),
  --         (const True Flow.<| undefined) Test.~?= True,
  --         fmap (Flow.apply' 2) [succ, recip, negate]
  --           Test.~?= [3, 0.5, -2 :: Double],
  --         Flow.apply undefined (const True) Test.~?= True,
  --         fmap (2 Flow.!>) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
  --         fmap (2 Flow.!>) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
  --         fmap (Flow.<! 2) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double]
  --       ]

  -- let hasErrors = Test.errors counts /= 0
  --     hasFailures = Test.failures counts /= 0
  -- Monad.when (hasErrors || hasFailures) Exit.exitFailure



maps1, maps2, maps3, maps4, maps5 :: [Double] -> [Double]
maps1 = func
maps2 = func Flow..> func
maps3 = func Flow..> func Flow..> func
maps4 = func Flow..> func Flow..> func Flow..> func
maps5 = func Flow..> func Flow..> func Flow..> func Flow..> func
pipeline :: Double -> Double
pipeline = (+ 1) Flow..> (* 10) Flow..> (/ 2)

func :: [Double] -> [Double]
func = map (+ 1) Flow..> map (+ 1) Flow..> map (+ 1) Flow..> map (+ 1) Flow..> map (+ 1) Flow..> map (+ 1) Flow..> map (+ 1) Flow..> map (+ 1) Flow..> map (+ 1) Flow..> map (+ 1) Flow..> map (+ 1)

maps1INLINE, maps2INLINE, maps3INLINE, maps4INLINE, maps5INLINE :: [Double] -> [Double]
maps1INLINE = funcINLINE
maps2INLINE = funcINLINE FlowINLINE..> funcINLINE
maps3INLINE = funcINLINE FlowINLINE..> funcINLINE FlowINLINE..> funcINLINE
maps4INLINE = funcINLINE FlowINLINE..> funcINLINE FlowINLINE..> funcINLINE FlowINLINE..> funcINLINE
maps5INLINE = funcINLINE FlowINLINE..> funcINLINE FlowINLINE..> funcINLINE FlowINLINE..> funcINLINE FlowINLINE..> funcINLINE
pipelineINLINE :: Double -> Double
pipelineINLINE = (+ 1) FlowINLINE..> (* 10) FlowINLINE..> (/ 2)

funcINLINE :: [Double] -> [Double]
funcINLINE = map (+ 1) FlowINLINE..> map (+ 1) FlowINLINE..> map (+ 1) FlowINLINE..> map (+ 1) FlowINLINE..> map (+ 1) FlowINLINE..> map (+ 1) FlowINLINE..> map (+ 1) FlowINLINE..> map (+ 1) FlowINLINE..> map (+ 1) FlowINLINE..> map (+ 1) FlowINLINE..> map (+ 1)

maps1NOINLINE, maps2NOINLINE, maps3NOINLINE, maps4NOINLINE, maps5NOINLINE :: [Double] -> [Double]
maps1NOINLINE = map pipelineNOINLINE
maps2NOINLINE = map pipelineNOINLINE FlowNOINLINE..> map pipelineNOINLINE
maps3NOINLINE = map pipelineNOINLINE FlowNOINLINE..> map pipelineNOINLINE FlowNOINLINE..> map pipelineNOINLINE
maps4NOINLINE = map pipelineNOINLINE FlowNOINLINE..> map pipelineNOINLINE FlowNOINLINE..> map pipelineNOINLINE FlowNOINLINE..> map pipelineNOINLINE
maps5NOINLINE = map pipelineNOINLINE FlowNOINLINE..> map pipelineNOINLINE FlowNOINLINE..> map pipelineNOINLINE FlowNOINLINE..> map pipelineNOINLINE FlowNOINLINE..> map pipelineNOINLINE
pipelineNOINLINE :: Double -> Double
pipelineNOINLINE = (+ 1) FlowNOINLINE..> (* 10) FlowNOINLINE..> (/ 2)

benchmark :: IO ()
benchmark = Cri.defaultMain
  [ Cri.bgroup "original"
      [ Cri.bench "1 map"  $ Cri.nf maps1 [0..1000000]
      , Cri.bench "2 maps" $ Cri.nf maps2 [0..1000000]
      , Cri.bench "3 maps" $ Cri.nf maps3 [0..1000000]
      , Cri.bench "4 maps" $ Cri.nf maps4 [0..1000000]
      , Cri.bench "5 maps" $ Cri.nf maps5 [0..1000000]
      ]
      
  , Cri.bgroup "INLINE"
      [ Cri.bench "1 map"  $ Cri.nf maps1INLINE [0..1000000]
      , Cri.bench "2 maps" $ Cri.nf maps2INLINE [0..1000000]
      , Cri.bench "3 maps" $ Cri.nf maps3INLINE [0..1000000]
      , Cri.bench "4 maps" $ Cri.nf maps4INLINE [0..1000000]
      , Cri.bench "5 maps" $ Cri.nf maps5INLINE [0..1000000]
      ]
  , Cri.bgroup "NOINLINE"
      [ Cri.bench "1 map"  $ Cri.nf maps1NOINLINE [0..1000000]
      , Cri.bench "2 maps" $ Cri.nf maps2NOINLINE [0..1000000]
      , Cri.bench "3 maps" $ Cri.nf maps3NOINLINE [0..1000000]
      , Cri.bench "4 maps" $ Cri.nf maps4NOINLINE [0..1000000]
      , Cri.bench "5 maps" $ Cri.nf maps5NOINLINE [0..1000000]
      ]

  ]
