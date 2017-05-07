{-# LANGUAGE OverloadedStrings #-}

module Frontend.Types where

data Node a = NotNode | Node a deriving (Show, Eq)