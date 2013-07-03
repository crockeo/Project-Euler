-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 19
-- Time started: 7/3/2013 - 12:33 AM
-- Time ended:
-------------------------------

-- Change the N to the current problem
module Euler19 where

import Data.Maybe
import Data.List

data Day = Saturday | Sunday | Monday | Tuesday | Wednesday | Thursday | Friday
  deriving (Eq, Ord, Enum, Show, Read)

toDay :: Int -> Day
toDay day = [Saturday .. Friday] !! day

data Month = March | April | May | June | July | August | September | October | November | December | January | February
  deriving (Eq, Ord, Enum, Show, Read)

toNum :: Month -> Int
toNum month = (fromJust (elemIndex month [March .. January])) + 3 

type Year = Int

calcDay :: Int -> Month -> Year -> Day
calcDay = undefined

-- Call the main function to get the result
main = undefined