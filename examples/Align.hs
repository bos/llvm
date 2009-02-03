module Align (main) where
import Data.TypeLevel(D4)
import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

main :: IO ()
main = do
    let td = ourTargetData
    print (littleEndian td,
           aBIAlignmentOfType td $ typeRef (undefined :: Word32),
	   aBIAlignmentOfType td $ typeRef (undefined :: Double),
	   aBIAlignmentOfType td $ typeRef (undefined :: Vector D4 Float),
	   storeSizeOfType td $ typeRef (undefined :: Vector D4 Float)
	   )
