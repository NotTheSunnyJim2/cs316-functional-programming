{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02Problems where

import Week02

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- In the questions below, replace 'undefined' with your answers. Use
   GHCi to test them.-}

{- 1. Write a function that counts the number of occurrences of an
      element in list: -}

popCount :: Eq a => a -> [a] -> Int
popCount _ [] = 0
popCount a (x:xs) | a == x = 1 + popCount a xs
                  | otherwise = popCount a xs
                  
{-    (popCount is short for "population count"). Examples:

         popCount 2 [1,2,5,2,7,2,9] == 3
         popCount 9 [1,2,5,2,7,2,9] == 1
         popCount 0 [1,2,5,2,7,2,9] == 0
-}


{- 2. Write a version of 'insert' that only inserts into a sorted list
      if the element is not already there. Examples:

         insertNoDup 2 [1,3,4]   == [1,2,3,4]
         insertNoDup 2 [1,2,3,4] == [1,2,3,4]
-}

insertNoDup :: Ord a => a -> [a] -> [a]
insertNoDup a [] = [a]
insertNoDup a (x:xs) | a == x = x:xs
                     | a < x = a:x:xs
                     | a > x = x : (insertNoDup a xs)

{- 3. Write a version of 'remove' that removes all copies of an element
      from a sorted list, not just the first one. Examples:

         removeAll 2 [1,2,2,3] == [1,3]
         removeAll 2 [1,3]     == [1,3]
-}

removeAll :: Ord a => a -> [a] -> [a]
removeAll a [] = []
removeAll a (x:xs) | a == x = removeAll a xs
                   | otherwise = x : removeAll a xs


{- 4. Rewrite 'treeFind' and 'treeInsert' to use 'compare' and 'case'
      expressions. -}

{-treeFind :: Ord k => k -> KV k v -> Maybe v
  treeFind k Leaf = Nothing
  treeFind k (Node l (k',v') r)
    | k == k'   = Just v'
    | k < k'    = treeFind k l
    | otherwise = treeFind k r
-}

treeFind2 :: Ord k => k -> KV k v -> Maybe v
treeFind2 k Leaf = Nothing
treeFind2 k (Node l (k', v') r) = 
      case compare k k' of
            EQ -> Just v'
            LT -> treeFind2 k l
            GT -> treeFind2 k r

{-
treeInsert :: Ord k => k -> v -> KV k v -> KV k v
treeInsert k v Leaf = Node Leaf (k,v) Leaf
treeInsert k v (Node l (k',v') r)
  | k == k'   = Node l (k,v) r
  | k < k'    = Node (treeInsert k v l) (k',v') r
  | otherwise = Node l (k',v') (treeInsert k v r)
-}

treeInsert2 :: Ord k => k -> v -> KV k v -> KV k v
treeInsert2 k v Leaf = Node Leaf (k,v) Leaf
treeInsert2 k v (Node l (k',v') r) =
      case compare k k' of
            EQ -> Node l (k,v) r
            LT -> Node (treeInsert2 k v l) (k',v') r
            GT -> Node l (k',v') (treeInsert2 k v r)


{- 5. MergeSort is another sorting algorithm that works in the following
      way:

      - If the list to be sorted is zero length, then it is already
        sorted.

      - If the list to be sorted has one element, then it is already
        sorted.

      - Otherwise, split the list into two, one with the even elements
        and one with the odd elements. Sort the two lists by calling
        'mergeSort' recursively. Then merge the two lists together
        maintaining the ordering.

      Write this function in three parts: -}

{-    'split' splits the input into two lists: one with the odd numbered
      elements and one with the even numbered elements. For example:

        > split [45,12,89,29,93]
        ([45,89,93],[12,29])

      HINT: you can pattern match on multiple elements at the head of
      a list with 'x1:x2:xs', and you can use the '(odds,evens) = ...'
      syntax in a 'where' clause. -}

split :: [a] -> ([a], [a])
split [] = ([], []) 
split [x] = ([x], [])
split (x1:x2:xs) = (x1:odds, x2:evens)
    where (odds, evens) = split xs

{-    'merge' merges two sorted lists into one sorted list. Examples:

          merge [1,3,5] [2,4,6]  = [1,2,3,4,5,6]
          merge [1,3,5] [7,9,11] = [1,3,5,7,9,11]
-}

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | x > y = y : merge (x:xs) ys

{-    'mergeSort' uses 'split' and 'merge' to implement the merge sort
      algorithm described above. -}

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs = merge (mergeSort xs1) (mergeSort xs2)
    where (xs1, xs2) = split xs


{- 6. Write another version of 'makeChange' that returns all the
      possible ways of making change as a list: 
      
makeChange :: [Coin] -> [Coin] -> Int -> Maybe [Coin]
makeChange coins        used 0 = Just used
makeChange []           used _ = Nothing
makeChange (coin:coins) used amount
  | amount >= coin =
    case makeChange coins (coin:used) (amount - coin) of
      Just coins -> Just coins
      Nothing    -> makeChange coins used amount
  | otherwise =
    makeChange coins used amount

-}

makeChangeAll :: [Coin] -> [Coin] -> Int -> [[Coin]] --list of available coins, list of used coins, target amount, list of all possible combos
makeChangeAll coins used 0 = [used]
makeChangeAll []    used _ = []
makeChangeAll (coin:coins) used amount
    | amount >= coin = makeChangeAll coins (coin:used) (amount - coin) ++ makeChangeAll coins used amount
    | otherwise = makeChangeAll coins used amount

{- HINT: you don't need a case expression, just a way of appending two
   lists of possibilities. -}

{- 7. This question involves converting between two datatypes. A 'Row'
      is a list of strings, such as you might find in a database: -}

-- | A row is a list of strings, one for each field. For example:
--
-- > ["Mount Snowden", "Wales"]
type Row = [String]

{-    Note that the names of the fields, which might be 'Mountain' and
      'Country' here, are implicit in this representation.

      The second type is a record, which is a list of pairs of field
      names with their data: -}

-- | A record is a list of fieldname / value pairs. For example:
--
-- > [("Mountain", "Mont Blanc"), ("Country", "France")]
type Record = [(String,String)]

{-    Implement the following functions on rows and records: -}

-- | Look up a field in a record, returning @Nothing@ if the field is
-- not in the record. For example,
-- > lookupField "a" [("a","1"),("b","2")]
-- returns @Just "1"@, but
-- > lookupField "c" [("a","1"),("b","3")]
-- returns @Nothing@.
lookupField :: String -> Record -> Maybe String
lookupField fieldname [] = Nothing 
lookupField fieldname ((k,v):records) | fieldname == k = Just v
                                      | otherwise = lookupField fieldname records

-- | Given a header listing field names, like:
--
-- >  ["Mountain", "Country"]
--
-- and a row like:
--
-- >   ["Ben Nevis", "Scotland"]
--
-- turn it into a record like:
--
-- >   [("Mountain", "Ben Nevis"), ("Country", "Scotland")]
--
-- If the number of field names in the header does not match the
-- number of fields in the row, an @Nothing@ should be returned.
rowToRecord :: [String] -> Row -> Maybe Record
rowToRecord [] [] = Just []
rowToRecord (hdr:hdrs) (rw:rws) = 
  case rowToRecord hdrs rws of
    Nothing -> Nothing
    Just record -> Just ((hdr,rw):record)
rowToRecord _ _ = Nothing

-- | Given a header listing field names, and a list of rows, converts
-- each row into a record. See 'rowToRecord' for how individual rows
-- are converted to records.
rowsToRecords :: [String] -> [Row] -> Maybe [Record]
rowsToRecords headers [] = Just []
rowsToRecords header (row:rows) =
  case rowsToRecords header rows of
    Nothing -> Nothing
    Just records ->
      case rowToRecord header row of
        Nothing -> Nothing
        Just record ->
          Just (record:records)

-- | Given a header listing field names, like:
--
-- >   ["Mountain", "Country"]
--
-- and a record like:
--
-- >   [("Mountain", "Ben Nevis"), ("Country", "Scotland")]
--
-- turn it into a row like:
--
-- >   ["Ben Nevis", "Scotland"]
--
-- It does not matter what order the fields in the record are in, so the
-- record:
--
-- >   [("Country", "Scotland"), ("Mountain", "Ben Nevis")]
--
-- should result in the same row.
--
-- This function returns an @Nothing@ if any of the field names listed in
-- the header are not in the record.
recordToRow :: [String] -> Record -> Maybe Row
recordToRow header record = 
  error "recordToRow: not implemented"

-- | Given a header listing field names, and a list of records,
-- converts each record into a row. See 'recordToRow' for how
-- individual records are converted to rows.
recordsToRows :: [String] -> [Record] -> Maybe [Row]
recordsToRows header records =
  error "recordsToRows: not implemented"
