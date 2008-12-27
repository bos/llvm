module Vector where
import Control.Monad.Trans(liftIO)
import Data.TypeNumbers
import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

type T = Float

type N = D1 (D6 End)

cgvec :: CodeGenModule (Function (T -> IO T))
cgvec = do
    f <- createFunction ExternalLinkage $ \ x -> do
        top1 <- getCurrentBasicBlock
        loop1 <- newBasicBlock
        body1 <- newBasicBlock
        exit1 <- newBasicBlock

        let v = value (zero :: ConstValue (Vector N T))
	    n = typeNumber (undefined :: N) :: Word32

	br loop1

        defineBasicBlock loop1
        i1 <- phi [(valueOf 0, top1)]
	x1 <- phi [(x, top1)]
	v1 <- phi [(v, top1)]
        t1 <- icmp IntULT i1 (valueOf n)
	condBr t1 body1 exit1
        defineBasicBlock body1
	x1' <- add x1 (1::T)
	i1' <- add i1 (1::Word32)
	v1' <- insertelement v1 x1 i1
	addPhiInputs i1 [(i1', body1)]
	addPhiInputs x1 [(x1', body1)]
	addPhiInputs v1 [(v1', body1)]
	br loop1
	defineBasicBlock exit1

	vsq <- mul v1 v1
        vcb <- mul vsq v1

	top2 <- getCurrentBasicBlock
        loop2 <- newBasicBlock
        body2 <- newBasicBlock
        exit2 <- newBasicBlock

	br loop2

        defineBasicBlock loop2
        i2 <- phi [(valueOf 0, top2)]
	s2 <- phi [(valueOf 0, top2)]
        t2 <- icmp IntULT i2 (valueOf n)
	condBr t2 body2 exit2
        defineBasicBlock body2
	i2' <- add i2 (1::Word32)
	y <- extractelement vcb i2
        s2' <- add s2 (y :: Value T)
	addPhiInputs i2 [(i2', body2)]
	addPhiInputs s2 [(s2', body2)]
	br loop2
	defineBasicBlock exit2

        ret (s2 :: Value T)

    liftIO $ dumpValue f
    return f

main :: IO ()
main = do
    m <- newModule
    iovec <- defineModule m cgvec
    writeBitcodeToFile "Vec.bc" m

    ee <- createModuleProviderForExistingModule m >>= createExecutionEngine
    let vec = unsafePurify $ generateFunction ee $ iovec

    print $ vec 10

