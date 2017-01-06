module Utils where

getInRange :: Float -> Int -> Int -> Int
getInRange random x y = fromIntegral (ceiling (numberRanged))
    where numberRanged = ((realToFrac(y-x)) * random) + realToFrac(x)

insert :: a -> Int -> [a] -> [a]
insert item index list = begin ++ (item:end)
    where (begin, end) = splitAt index list

delete :: Int -> [a] -> [a]
delete index list = (take index list) ++ (drop (index+1) list)

getAndDelete :: Int -> [a] -> (a, [a])
getAndDelete index list = (list!!index, (take index list) ++ (drop (index+1) list))

edit :: Int -> a -> [a] -> [a]
edit index item list = insert item index (delete index list)

editAndSave :: Int -> a -> [a] -> (a,[a])
editAndSave index item list = (list!!index, edit index item list)

mix :: [a] -> [Float] -> [a]
mix list random = mix' list [] random 

mix' :: [a] -> [a] -> [Float] -> [a]
mix' [] newList _ = newList
mix' list newList (randomNotRanged:restRandom) = mix' restList newList' restRandom
    where   randomIndex = Utils.getInRange randomNotRanged 0 (length list)-1
            newList' = item:newList
            (item, restList) = Utils.getAndDelete randomIndex list