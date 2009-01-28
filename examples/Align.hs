module Align (main) where
import Data.TypeNumbers
import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

main :: IO ()
main = do
    let td = ourTargetData
    print (littleEndian td,
           aBIAlignmentOfType td $ typeRef (undefined :: Word32),
	   aBIAlignmentOfType td $ typeRef (undefined :: Double),
	   aBIAlignmentOfType td $ typeRef (undefined :: Vector (D4 End) Float),
	   storeSizeOfType td $ typeRef (undefined :: Vector (D4 End) Float)
	   )
