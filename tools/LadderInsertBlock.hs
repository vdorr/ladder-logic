#!/usr/bin/runghc
{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns -fno-warn-tabs #-}

{-

function! InsertFunctionBlock( name, ... )

  let x = col(".")
  let y = line(".")

  let save_pos = getpos(".")

"  execute "%!LadderInsertBlock.hs " . " " y . " " x . " " . a:name
  execute "%!LadderInsertBlock.hs " . " " y . " " x . " " . a:name join(a:000)
  call setpos(".", save_pos)

endfunction

command -nargs=* FB :call InsertFunctionBlock(<f-args>)



-}

import System.IO (isEOF)
import System.Environment (getArgs)
import Data.Char (toUpper)
import Control.Monad
import Text.Read

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
		[ "+-----+"
		, "| TON |"
		, "|IN  Q|"
		, "|PT ET|"
		, "+-----+"
		]
		))
	]

mkBlock :: String -> [String] -> [String] -> [String]
mkBlock name ins outs = header ++ body ++ [cap]
	where
	w_l = (maximum (fmap (length.inName) ins))
	w_r = maximum $ fmap length outs
	w_n = length name
	w = max (1 + w_l + w_r) (2 + w_n)
	cap = "+" ++ replicate w '-' ++ "+"
	header =
		[ cap
		, "|" ++ replicate hdr_pad ' ' ++ name
			++ replicate (w - w_n - hdr_pad) ' ' ++ "|" ]
	hdr_pad = (w - w_n) `div` 2
	body = zipWith f
		(ins ++ replicate (length outs - length ins) "")
		(outs ++ replicate (length ins - length outs) "")
	f i o = input i ++ io_fill i o ++ o ++ "|"

	input i@('<':_) = i ++ " "
	input i@('>':_) = i ++ " "
	input i = '|' : i

	inName ('<':i) = i
	inName ('>':i) = i
	inName i = i

	io_fill i o
		= replicate (w - length i - length o)
			(if i == o && not (null i) then '-' else ' ')

--------------------------------------------------------------------------------

main :: IO ()
main = do
	args <- getArgs
	case args of
		l':c':a | Just l <- readMaybe l', Just c <- readMaybe c' -> do
			case a of
				[block] | Just (pivot, blck) <- lookup (fmap toUpper block) blocks
					-> insertBlock (l - 1) c blck pivot
				block:rest ->
					case break (=="/") rest of
						(ins, "/" : outs)
							-> insertBlock (l - 1) c (mkBlock block ins outs) 2
						(ins, []) -> undefined
						_ -> return ()
				_ -> return ()
		_ -> do
			return ()
-- 			print args
-- 			forM_ (mkBlock "A" ["a"] ["b"]) putStrLn
-- 			forM_ (mkBlock "TON" ["in", "pt"] ["Q", "et"]) putStrLn
-- 			forM_ (mkBlock "INC" ["", "V"] ["A", "V"]) putStrLn
-- 			forM_ (mkBlock "INC" ["", "V"] ["A", "V"]) putStrLn
-- 			forM_ (mkBlock "CTU" ["CU", "R", "PV"] ["Q", "", "CV"]) putStrLn
-- 			forM_ (mkBlock "CTU" [">CU", "R", "PV"] ["Q", "", "CV"]) putStrLn
-- 			forM_ (mkBlock "ADD" ["", ""] [""]) putStrLn

-- 		line : col : (block : _) -> do
-- 			let Just (pivot, blck) = lookup (fmap toUpper block) blocks
-- 			insertBlock (read line - 1) (read col) blck pivot
