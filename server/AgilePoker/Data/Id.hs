module AgilePoker.Data.Id (generateId) where

import Data.ByteString (ByteString)
import Servant (FromHttpApiData(..))
import Data.Map.Strict (Map)
import System.Random (randomRs, newStdGen)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (liftM)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


instance FromHttpApiData ByteString where
  parseUrlPiece bs = Right $ TE.encodeUtf8 bs


randString :: IO String
randString =
    liftM (take 32 . randomRs ('a','z')) newStdGen


generateId :: Map ByteString a -> IO ByteString
generateId m = do
  newId <- encodeUtf8 . T.pack <$> randString
  if Map.member newId m then
    generateId m
  else
    pure newId
