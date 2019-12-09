-- solution for
-- http://adventofcode.com/2019/day/1


module Main where

import Util.Main1 (main12)

-- ----------------------------------------

main :: IO ()
main = main12 "2019-01"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

solve1 :: [Int] -> Int
solve1 = sum . map fuel

solve2 :: [Int] -> Int
solve2 = sum . map (sum . takeWhile (/= 0) . drop 1 . iterate fuel)

fuel :: Int -> Int
fuel x = (x `div` 3 - 2) `max` 0

fromString :: String -> [Int]
fromString = map read . lines

-- ----------------------------------------

inp :: String
inp = "141255\n93769\n122572\n149756\n72057\n82031\n60702\n124099\n135752\n115450\n132370\n80491\n95738\n143495\n136550\n104549\n94588\n106213\n60223\n78219\n60082\n118735\n127069\n101492\n110708\n141521\n117201\n61006\n117919\n90301\n114235\n114314\n95737\n119737\n86881\n86544\n114809\n142720\n138854\n144712\n133167\n87144\n106932\n111031\n112390\n109664\n66068\n50997\n141775\n73637\n121700\n64790\n127751\n100007\n62234\n75611\n57855\n135729\n83746\n139042\n112117\n76456\n129343\n50490\n146912\n77074\n147598\n149476\n101984\n85490\n128768\n70178\n98111\n118362\n129962\n66553\n76347\n140614\n127431\n110969\n138104\n118212\n107207\n79938\n144751\n142226\n116332\n65844\n124779\n104634\n83143\n94999\n72677\n76926\n83956\n59617\n145999\n62619\n87955\n143030\n"

res1, res2 :: Int
res1 = 3497399
res2 = 5243207

-- ----------------------------------------
