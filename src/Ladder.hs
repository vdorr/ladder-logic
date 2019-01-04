{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-tabs -fwarn-incomplete-patterns
                     -fwarn-unused-binds
                     -fwarn-unused-imports #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Ladder where

import DiagramParser

main = do
	print here

