module Align (main) where
import Data.TypeLevel(D1, D2, D4)
import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

main :: IO ()
main = do
    -- Initialize jitter
    initializeNativeTarget

    td <- getTargetData
    print (littleEndian td,
           aBIAlignmentOfType td $ typeRef (undefined :: Word32),
           aBIAlignmentOfType td $ typeRef (undefined :: Word64),
	   aBIAlignmentOfType td $ typeRef (undefined :: Vector D4 Float),
	   aBIAlignmentOfType td $ typeRef (undefined :: Vector D1 Double),
	   storeSizeOfType td $ typeRef (undefined :: Vector D4 Float),
           intPtrType td
	   )
