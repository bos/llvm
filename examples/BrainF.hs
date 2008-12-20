module BrainF where
-- BrainF compiler example 
--
--
-- The BrainF language has 8 commands:
-- Command   Equivalent C    Action
-- -------   ------------    ------
-- ,         *h=getchar();   Read a character from stdin, 255 on EOF
-- .         putchar(*h);    Write a character to stdout
-- -         --*h;           Decrement tape
-- +         ++*h;           Increment tape
-- <         --h;            Move head left
-- >         ++h;            Move head right
-- [         while(*h) {     Start loop
-- ]         }               End loop
--
import Control.Monad.Trans
import Data.Word
import Data.Int

import LLVM.Core
import LLVM.ExecutionEngine

main :: IO ()
main = do
--    text <- getContents
    let text = "+++++++++++++++++++++++++++++++++" ++  -- constant 33
               ">++++" ++                              -- next cell, loop counter, constant 4
               "[>++++++++++" ++                       -- loop, loop counter, constant 10
                 "[" ++                                -- loop
                   "<<.+>>-" ++                        -- back to 33, print, increment, forward, decrement loop counter
                 "]<-" ++                              -- back to 4, decrement loop counter
               "]" ++
               "++++++++++."

    prog <- simpleFunction $ brainMod text 65536
    putStrLn "Should print !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGH on the next line:"
    prog

brainMod :: String -> Word32 -> CodeGenModule (Function (IO ()))
brainMod instrs wmemtotal = do
    -- LLVM functions
    memset    <- newNamedFunction ExternalLinkage "llvm.memset.i32"
              :: TFunction (Ptr Word8 -> Word8 -> Word32 -> Word32 -> IO ())
    getchar   <- newNamedFunction ExternalLinkage "getchar"
              :: TFunction (IO Int32)
    putchar   <- newNamedFunction ExternalLinkage "putchar"
              :: TFunction (Int32 -> IO Int32)


    let generate [] [] _ =
            return ()
        generate [] (_:_) _ = error "Missing ]"
        generate (']':_) [] _ = error "Missing ["
        generate (']':is) ((cphi, loop, exit) : bs) (cur, bb) = do
            addPhiInputs cphi [(cur, bb)]
            br loop
            defineBasicBlock exit
            generate is bs (cphi, exit)
    
        generate ('[':is) bs curbb = do
            loop <- newBasicBlock
            body <- newBasicBlock
            exit <- newBasicBlock
            br loop

            defineBasicBlock loop
            cur <- phi [curbb]
            val <- load cur
            eqz <- icmp IntEQ val (0::Word8)
            condBr eqz exit body
    
            defineBasicBlock body
            generate is ((cur, loop, exit) : bs) (cur, body)
    
        generate (i:is) bs (curhead, bb) = do
            curhead' <- gen curhead i
            generate is bs (curhead', bb)

        gen cur ',' = do
            char32 <- call getchar
            char8  <- trunc char32
            store char8 cur
            return cur
        gen cur '.' = do
            char8 <- load cur
            char32 <- zext char8
            call putchar char32
            return cur
        gen cur '-' = do
            val <- load cur
            val' <- sub val (1 :: Word8)
            store val' cur
            return cur
        gen cur '+' = do
            val <- load cur
            val' <- add val (1 :: Word8)
            store val' cur
            return cur
        gen cur '<' =
            getElementPtr cur ((-1) :: Word32, ())
        gen cur '>' =
            getElementPtr cur (1 :: Word32, ())
        gen _ c = error $ "Bad character in program: " ++ show c


    brainf <- createFunction InternalLinkage $ do
        let memtotal = valueOf wmemtotal
        ptr_arr <- arrayMalloc memtotal
        call memset ptr_arr (valueOf 0) memtotal (valueOf 0)
--        _ptr_arrmax <- getElementPtr ptr_arr [memtotal]
        curhead <- getElementPtr (ptr_arr :: Value (Ptr Word8)) (valueOf (wmemtotal `div` 2), ())

        bb <- getCurrentBasicBlock
        generate instrs [] (curhead, bb)

        free ptr_arr
        ret ()

    liftIO $ dumpValue brainf

    return brainf
