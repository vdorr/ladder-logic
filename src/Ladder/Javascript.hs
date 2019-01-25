{-# LANGUAGE CPP, OverloadedStrings, TupleSections, MultiParamTypeClasses #-}

#define here (__FILE__ <> ":" <> show (__LINE__ :: Integer) <> " ")

module Ladder.Javascript where

import qualified Data.Text as T
import Data.Text (Text, append, pack)
import qualified Data.Map as M
import Data.List
import Data.Traversable
import Data.Foldable

import Algebra.Graph.AdjacencyMap.Algorithm

import Compile
import LadderParser
-- import DiagramParser

--------------------------------------------------------------------------------

data JSRef a = JSRef Text
	deriving Show

data JS a = JS Text
	deriving Show

instance Ref JSRef JS where
	readRef (JSRef r) = JS $ "env.get(" <> r <> ")"
	joinWires (JSRef pwr) (JSRef nr) = JS
		$ "env.set(" <> nr <> ", " <> "env.get(" <> pwr <> ") || env.get(" <> nr <> "))"

--------------------------------------------------------------------------------

jsget (JSRef r) = "env.get("<> r <> ")"
jsset (JSRef r) v = "env.set("<> r <> ", " <> v <> ")"
jsif pwr a
	= "if ( " <> jsget pwr <> " ) { "
	<> a --"env.set(" <> a' <> ", " <> f pwr <> "); "
	<> "}"

jsdevices :: DeviceTable lbl JS JSRef (Either Text) xx

-- jsdevices :: [(String,
--                         (Int,
--                          [(a, (JSRef Bool, JSRef Bool))]
--                          -> JSRef Bool -> m (JSRef Bool, [Pg lbl JS ()])))]
jsdevices =
	[ dev "[ ]" 1 $ \[(_, (a, _))] -> update
		(\pwr -> jsset pwr $ jsget pwr <> " && " <> jsget a)
 	, dev "[/]" 1 $ \[(_, (a, _))] -> update
		(\pwr -> jsset pwr $ jsget pwr <> " && !(" <> jsget a <> ")")
	, dev "( )" 1 $ \[a] -> update (\pwr -> jsset pwr (jsget pwr))
	, dev "(S)" 1 $ \[(_, (_, a'))] -> update (\pwr -> jsif pwr (jsset a' "true") )
	, dev "(R)" 1 $ \[(_, (_, a'))] -> update (\pwr -> jsif pwr (jsset a' "false") )
	]
	where
	dev n na f = (n, (na, f))
	update f pwr = return (pwr, [Do $ JS $ f pwr])


generatejs :: [(Maybe String, Symbol)] -> Either Text Text
generatejs nets = do

-- 	print (here, toList $ gatherVariables nets)

	let vars =
		fmap (\v -> (v, (JSRef $ "\"in " <> v <> "\"", JSRef $ "\"out " <> v <> "\"")))
		$ toList $ gatherVariables nets
-- 	print (here, vars)

	let nodes =
		fmap (\p -> (p, JSRef $ "\"node " <> pack (show p) <> "\""))
		$ toList
		$ foldMap (gatherNodes . snd) nets

	jsAL <- forM nets $ \(lbl, net) -> do
		let ff = fmap (\net -> (JSRef "\"pwr\"", net)) $ dfsForest $ jjj_ net

		(lbl,) <$> xxxxXxx jsdevices (M.fromList vars) (M.fromList nodes) ff

	let env = makeJSEnv vars nodes
-- 	print (here)
	(env <>) <$> tojs jsAL
-- 	error here


makeJSEnv vars nodes = T.unlines $ prologue <> e
	where
	prologue = ["env.m[\"pwr\"] = true;"]
	e = fmap ((<>";") . f)
		(let (a, b) = unzip (fmap snd vars) in (a<>b) <> fmap snd nodes)
	f (JSRef v) = "env.m[" <> v <> "] = false"

--------------------------------------------------------------------------------

-- | turn list of JS action to actual JS source text fragment
tojs :: Eq lbl => [(lbl, [Pg lbl JS ()])] -> Either Text Text
tojs program = do
	return $ loop $ foldMap f program
	where
	f (lbl, a) = (T.concat ["case ", pack $ show (getLbl lbl), ":"])
								  : foldMap (fmap (append "  ") . h) a
	h (Do (JS c)) = [c <> ";"]
	h (Go lbl) = ["target = " <> pack (show (getLbl lbl)) <> ";", "continue;"]
	h (Br lbl (JS c)) =
		[ "if ( " <> c <> " ) {"
		, "  target = " <> pack (show (getLbl lbl)) <> ";"
		, "  continue;"
		, "}"
		]
	loop stmts = T.unlines
		["{"
		, "  var target = 0;"
		, "  while ( env.run() ) {"
		, "    env.scan_begin();"
		, "    switch ( target ) {"
		, T.unlines $ fmap (append "      ") stmts
		, "    }"
		, "    env.scan_end();"
		, "  }"
		, "}"
		]
	getLbl lbl = maybe (error "label not found") id $ findIndex ((lbl==).fst) program

--------------------------------------------------------------------------------
