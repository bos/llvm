module BrainF where
-- BrainF compiler example 
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
import Control.Monad(when)
import Data.Word
import Data.Int
import System.Environment(getArgs)

import LLVM.Core
import LLVM.Util.File(writeCodeGenModule)
import LLVM.ExecutionEngine

main :: IO ()
main = do
    -- Initialize jitter
    initializeNativeTarget

    aargs <- getArgs
    let (args, debug) = if take 1 aargs == ["-"] then (tail aargs, True) else (aargs, False)
    let text = "+++++++++++++++++++++++++++++++++" ++  -- constant 33
               ">++++" ++                              -- next cell, loop counter, constant 4
               "[>++++++++++" ++                       -- loop, loop counter, constant 10
                 "[" ++                                -- loop
                   "<<.+>>-" ++                        -- back to 33, print, increment, forward, decrement loop counter
                 "]<-" ++                              -- back to 4, decrement loop counter
               "]" ++
               "++++++++++."
    prog <- if length args == 1 then readFile (head args) else return text

    when (debug) $
        writeCodeGenModule "BrainF.bc" $ brainCompile debug prog 65536

    bfprog <- simpleFunction $ brainCompile debug prog 65536
    when (prog == text) $
        putStrLn "Should print '!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGH' on the next line:"
    bfprog

brainCompile :: Bool -> String -> Word32 -> CodeGenModule (Function (IO ()))
brainCompile _debug instrs wmemtotal = do
    -- LLVM functions
    memset    <- newNamedFunction ExternalLinkage "llvm.memset.i32"
              :: TFunction (Ptr Word8 -> Word8 -> Word32 -> Word32 -> IO ())
    getchar   <- newNamedFunction ExternalLinkage "getchar"
              :: TFunction (IO Int32)
    putchar   <- newNamedFunction ExternalLinkage "putchar"
              :: TFunction (Int32 -> IO Int32)

    -- Generate code, first argument is the list of commands,
    -- second argument is a stack of loop contexts, and the
    -- third argument is the current register for the head and
    -- the current basic block.
    -- A loop context is a triple of the phi node, the loop top label,
    -- and the loop exit label.
    let generate [] [] _ =
            return ()
        generate [] (_:_) _ = error "Missing ]"
        generate (']':_) [] _ = error "Missing ["
        generate (']':is) ((cphi, loop, exit) : bs) (cur, bb) = do
            -- The loop has terminated, add the phi node at the top,
            -- branch to the top, and set up the exit label.
            addPhiInputs cphi [(cur, bb)]
            br loop
            defineBasicBlock exit
            generate is bs (cphi, exit)
    
        generate ('[':is) bs curbb = do
            -- Start a new loop.
            loop <- newBasicBlock    -- loop top
            body <- newBasicBlock    -- body of the loop
            exit <- newBasicBlock    -- loop exit label
            br loop

            defineBasicBlock loop
            cur <- phi [curbb]       -- will get one more input from the loop terminator.
            val <- load cur          -- load head byte.
            eqz <- icmp IntEQ val (0::Word8) -- test if it is 0.
            condBr eqz exit body     -- and branch accordingly.
    
            defineBasicBlock body
            generate is ((cur, loop, exit) : bs) (cur, body)
    
        generate (i:is) bs (curhead, bb) = do
            -- A simple command, with no new basic blocks.
            -- Just update which register the head is in.
            curhead' <- gen curhead i
            generate is bs (curhead', bb)

        gen cur ',' = do
            -- Read a character.
            char32 <- call getchar
            char8  <- trunc char32
            store char8 cur
            return cur
        gen cur '.' = do
            -- Write a character.
            char8 <- load cur
            char32 <- zext char8
            _ <- call putchar char32
            return cur
        gen cur '-' = do
            -- Decrement byte at head.
            val <- load cur
            val' <- sub val (1 :: Word8)
            store val' cur
            return cur
        gen cur '+' = do
            -- Increment byte at head.
            val <- load cur
            val' <- add val (1 :: Word8)
            store val' cur
            return cur
        gen cur '<' =
            -- Decrement head.
            getElementPtr cur ((-1) :: Word32, ())
        gen cur '>' =
            -- Increment head.
            getElementPtr cur (1 :: Word32, ())
        gen _ c = error $ "Bad character in program: " ++ show c


    brainf <- createFunction ExternalLinkage $ do
        ptr_arr <- arrayMalloc wmemtotal
        _ <- call memset ptr_arr (valueOf 0) (valueOf wmemtotal) (valueOf 0)
--        _ptr_arrmax <- getElementPtr ptr_arr (wmemtotal, ())
        -- Start head in the middle.
        curhead <- getElementPtr ptr_arr (wmemtotal `div` 2, ())

        bb <- getCurrentBasicBlock
        generate instrs [] (curhead, bb)

        _ <- free ptr_arr
        ret ()

    return brainf
