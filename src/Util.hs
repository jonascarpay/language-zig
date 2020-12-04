module Util where

import AST
import Data.ByteString qualified as BS
import Syntax
import Text.Megaparsec

parseZig :: FilePath -> IO StructDef
parseZig fp = do
  bs <- BS.readFile fp
  either (fail . errorBundlePretty) pure $ runParser pZig fp bs
