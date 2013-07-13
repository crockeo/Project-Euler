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

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Enum, Show, Read)
type DayNum = Int
data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving (Eq, Ord, Enum, Show, Read)
type Year = Int

type Date = (DayNum, Day, Month, Year)

nextDay :: Day -> Day
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

nextMonth :: Month -> Month
nextMonth January   = February
nextMonth February  = March
nextMonth March     = April
nextMonth April     = May
nextMonth May       = June
nextMonth June      = July
nextMonth July      = August
nextMonth August    = September
nextMonth September = October
nextMonth October   = November
nextMonth November  = December
nextMonth December  = January

monthLength :: Month -> Year -> Int
monthLength January   year = 31
monthLength February  year =
  if isLeapYear year
    then 29
    else 28
monthLength March     year = 31
monthLength April     year = 30
monthLength May       year = 31
monthLength June      year = 30
monthLength July      year = 31
monthLength August    year = 31
monthLength September year = 30
monthLength October   year = 31
monthLength November  year = 30
monthLength December  year = 31

isLeapYear :: Year -> Bool
isLeapYear year = ((year `mod` 4 == 0) && (year `mod` 100 /= 0)) || (year `mod` 400 == 0)

nextDate :: Date -> Date
nextDate date@(daynum, day, month, year)
  | (nextdaynum > monthLength month year) && (nextmonth == January) = (1, nextday, nextmonth, nextyear)
  | (nextdaynum > monthLength month year) = (1, nextday, nextmonth, year)
  | otherwise = (nextdaynum, nextday, month, year)
  where nextdaynum = daynum + 1
        nextday    = nextDay day
        nextmonth  = nextMonth month
        nextyear   = year + 1

isTargetDay :: Date -> Bool
isTargetDay (1, Sunday, month, year) = True
isTargetDay date = False

calcLoop :: Date -> Int
calcLoop date@(daynum, day, month, 2001) = 0
calcLoop date
  | isTargetDay date = 1 + (calcLoop $ nextDate date)
  | otherwise        = calcLoop $ nextDate date

-- Call the main function to get the result
main = calcLoop (1, Tuesday, January, 1901)