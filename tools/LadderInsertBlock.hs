#!/usr/bin/runghc
{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns -fno-warn-tabs #-}

{-

function! InsertFunctionBlock( name )

  let x = col(".")
  let y = line(".")

   let save_pos = getpos(".")
"  call append('.', '') 

  execute "%!LadderInsertBlock.hs " . " " y . " " x . " " . a:name
   call setpos(".", save_pos)

endfunction

command -nargs=* FB :call InsertFunctionBlock(<f-args>)


-}

import System.IO (isEOF)
import System.Environment (getArgs)
import Data.Char (toUpper)
import Control.Monad

pass :: IO ()
pass = do
	end <- isEOF
	if end then return ()
	else (getLine >>= putStrLn >> pass)

goto :: Int -> IO ()
goto ln = do
	if ln > 0 then (getLine >>= putStrLn >> goto (ln - 1))
	else return ()

putSnippetLn :: Int -> String -> IO ()
putSnippetLn co snip = do
	end <- isEOF
	original <- if end then return "" else getLine
	let orig = original ++ replicate (co - length original) ' '
	let (a, b) = splitAt co orig
	let (_, c) = splitAt (length snip) b --insert or overwrite?
	putStrLn $ a ++ snip ++ c

insertBlock :: Int -> Int -> [String] -> Int -> IO ()
insertBlock ln co blck pivot = do
	goto (ln - pivot)
	forM_ blck (putSnippetLn co)
	pass

blocks :: [(String, (Int, [String]))]
blocks = 
	[ ("TON", (2,
		[ "+--------+"
		, "|  TON   |"
		, "|IN     Q|"
		, "|PT    ET|"
		, "+--------+"
		]
		))
	]

main :: IO ()
main = do
	[line, col, block] <- getArgs
	let Just (pivot, blck) = lookup (fmap toUpper block) blocks
	insertBlock (read line - 1) (read col) blck pivot

