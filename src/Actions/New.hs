{-# LANGUAGE OverloadedStrings #-}

module Actions.New (new) where

import UserMessages (choseATemplate)
import Config (get)
import Questions (select, Option)

new :: IO ()
new = do template <- select
                       choseATemplate [
                       "foo", "bar", "foo"
                     ]
         print "all done"
         return ()
