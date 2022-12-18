{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module BooleanLayout (BooleanLayout (..)) where

import           XMonad
import qualified XMonad.StackSet as W

data BooleanLayout l1 l2 a = BooleanLayout Bool (l1 a) (l2 a)
    deriving (Read, Show)

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (BooleanLayout l1 l2) a where
    runLayout (W.Workspace i p@(BooleanLayout bool lt lf) ms) r 
        | bool = do
            (wrs, _mlt') <- runLayout (W.Workspace i lt ms) r
            return (wrs, Just p)
        | otherwise = do
            (wrs, _mlt') <- runLayout (W.Workspace i lf ms) r
            return (wrs, Just p)

    handleMessage (BooleanLayout bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ BooleanLayout bool nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (\nf -> return . Just $ BooleanLayout bool lt nf)

    description (BooleanLayout True  l1 _) = description l1
    description (BooleanLayout _     _ l2) = description l2

