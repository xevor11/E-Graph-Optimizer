module Optimizer where

import Egg

-- Define a type for the optimizer
newtype Optimizer = Optimizer {
  optimize :: Program -> Program
}

-- Define a function to create a new optimizer
newOptimizer :: IO Optimizer
newOptimizer = do
  -- Create a new e-graph
  let egraph = emptyEGraph

  -- Define the optimization rules
  let rules = [
        rw!("commute-add"; "(+ ?x ?y)" => "(+ ?y ?x)"),
      ]

  -- Define the optimize function
  let optimizeFn prog = do
        -- Add each node in the program to the e-graph
        let graph = foldl addNode egraph prog

        -- Apply the optimization rules to the e-graph
        let optimizedGraph = applyRules rules graph

        -- Extract the optimized program from the e-graph
        let optimizedProg = extract optimizedGraph

        -- Return the optimized program
        optimizedProg

  -- Return the optimizer
  return $ Optimizer optimizeFn

-- Define a function to apply the optimizer to a program
runOptimizer :: Optimizer -> Program -> Program
runOptimizer optimizer prog = optimize optimizer prog
