{-# LANGUAGE TemplateHaskell #-}

import Data.Time.Calendar

import qualified Data.Map.Strict as Map

type HalfLife = Integer -- Days

data Community = Community {
  halfLife :: HalfLife -- The karma half life
  wallet :: Integer -- Number of credits that can be given without withdrawal
  }

data Commit = Commit Bytestring Integer From To Time

data Balance = Balance Integer Time

// UIDS are stored in an email folder
data Summary = Map UID Balance

data Period = Integer -- Number of days

exchange giver reciever

use typeclass
age balance
age summary

karmaDecay :: Integer -> Period -> HalfLife -> Integer
karmaDecay amount days halfLife = amount * (0.5) ** (days / halfLife)

mergeSummary :: Summary -> [Commit]
mergeSummary summ commits = foldl mergeSummary (age summ) commits

mergeSummary :: Summary -> Commit
mergeSummary accum commit = 
