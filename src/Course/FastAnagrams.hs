{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams s f =
  (\t -> map ncString $ listh . S.toList $ S.intersection 
          (S.fromList . hlist . (NoCaseString `map`) . lines $ t)
          (S.fromList . hlist . (NoCaseString `map`) . permutations $ s)) <$>
  readFile f

  -- readFile f

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

instance Ord NoCaseString where
  (<=) = (<=) `on` map toLower . ncString
