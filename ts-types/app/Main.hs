module Main where

import Data.PartialOrd (PartialOrd, (<=))
import Data.Set (Set, fromList, isSubsetOf, size, toList)

data TSPrimitiveType
  = TSString
  | TSNumber
  | TSBoolean
  | TSNull
  | TSUndefined
  deriving (Show, Eq)

-- The information about type of a property in a TypeScript object
data TSProperty
  = ObjectProperty TSProperty
  | PrimitiveProperty TSPrimitiveType
  | ObjectArrayProperty [TSProperty]
  | PrimitiveArrayProperty [TSPrimitiveType]
  deriving (Show, Eq)

type PropertyRange = Set String

-- TypeScript object may have properties with specific types
newtype TSObjectType = TSObjectType
  { table :: (PropertyRange, String -> Maybe TSProperty)
  }

data TSType
  = Primitive TSPrimitiveType
  | TSArray TSType
  | TSObject TSObjectType
  deriving (Show, Eq)

instance Show TSObjectType where
  show (TSObjectType objType) = "TSObject" <> show (helper objType)
   where
    helper (keys, f) =
      let props = map (\k -> (k, f k)) (toList keys)
       in props

instance Eq TSObjectType where
  (TSObjectType (keys1, f1)) == (TSObjectType (keys2, f2)) =
    keys1 == keys2 && all (\k -> f1 k == f2 k) (toList keys1)

instance PartialOrd TSObjectType where
  (TSObjectType (keys1, f1)) <= (TSObjectType (keys2, f2)) =
    keys1 `isSubsetOf` keys2
      && all (\k -> f1 k == f2 k) (toList keys1)

tsConstructObject :: [(String, TSProperty)] -> Maybe TSObjectType
tsConstructObject [] = Nothing
tsConstructObject props = do
  keys <- fromList' $ map fst props
  return $ TSObjectType (keys, lookupType)
 where
  lookupType :: String -> Maybe TSProperty
  lookupType k = lookup k props

fromList' :: (Ord a) => [a] -> Maybe (Set a)
fromList' [] = Nothing
fromList' xs =
  let xs' = fromList xs
   in if length xs /= size xs' then Nothing else Just xs'

main :: IO ()
main = do
  print a
  print b
  print $ (Data.PartialOrd.<=) <$> b <*> a
  print $ (Data.PartialOrd.<=) <$> a <*> b
a :: Maybe TSObjectType
a = tsConstructObject [("foo", PrimitiveProperty TSString), ("bar", PrimitiveProperty TSNumber)]
b :: Maybe TSObjectType
b = tsConstructObject [("foo", PrimitiveProperty TSString)]
