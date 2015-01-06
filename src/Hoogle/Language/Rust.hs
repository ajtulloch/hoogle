{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Hoogle.Language.Rust(parseInputRust) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Hoogle.Type.All

data RustCrate = RustCrate { crate :: [RustEntry] } deriving (Show)
data RustEntry = RustEntry { name :: String, signature :: RustSignature } deriving (Show)
data RustSignature = RustSignature {ty :: RustType } deriving (Show)

data RustType = RustFn [RustType] | RustApp RustType [RustType] | RustLit String | RustVar String deriving (Show)

parseVar :: Value -> Parser RustType
parseVar v@(Array a) =
  case V.head a of
    String a -> return $ RustVar (T.unpack a)

parseLit :: Value -> Parser RustType
parseLit v@(Array a) =
  case V.head a of
    String a -> return $ RustLit (T.unpack a)

parseFun :: Value -> Parser RustType
parseFun v@(Array a) =
  case V.head a of
    (Array args) -> do
                  tyArgs <- mapM parseRustType (V.toList args)
                  return $ RustFn tyArgs

parseApp :: Value -> Parser RustType
parseApp v@(Array a) =
  case (V.head a, V.head (V.tail a)) of
    (ty, Array args) -> do
      tyArgs <- mapM parseRustType (V.toList args)
      ty <- parseRustType ty
      return $ RustApp ty tyArgs

toType :: RustType -> Type
toType (RustFn args) = TFun (map toType args)
toType (RustApp ty args) = TApp (toType ty) (map toType args)
toType (RustLit lit) = TLit lit
toType (RustVar var) = TVar var

parseRustType :: Value -> Parser RustType
parseRustType v@(Object o)= do
  variant <- o .: "variant"
  fields <- o .: "fields"
  case variant of
    String "Lit" -> parseLit fields
    String "Var" -> parseVar fields
    String "Fun" -> parseFun fields
    String "App" -> parseApp fields

instance FromJSON RustType where
    parseJSON = parseRustType

$(deriveFromJSON defaultOptions ''RustCrate)
$(deriveFromJSON defaultOptions ''RustEntry)
$(deriveFromJSON defaultOptions ''RustSignature)

parseInputRust :: String -> ([ParseError], Input)
parseInputRust content = ([], ([], items))
    where
      Just (RustCrate {crate=entries}) = decode (BS.pack content) :: Maybe RustCrate
      items = map rustEntryToItem entries

testRust :: IO ()
testRust = do
  contents <- BS.readFile "/Users/tulloch/src/tullochtemp/x/hoogle.json"
  print (decode contents :: Maybe RustCrate)

rustEntryToItem :: RustEntry -> TextItem
rustEntryToItem (RustEntry {name=name, signature=RustSignature{ty=rty}}) =
    TextItem{
  itemLevel = 2,
  itemKind=FunctionItem,
  itemKey=name,
  itemName=name,
  itemType=Just (TypeSig [] ty),
  itemDisp=Tags [bold name, space, emph "::", space, bold (show ty)],
  itemURL="#v:name",
  itemDocs = "",
  itemPriority=2
}
    where
      ty = toType rty
      emph = TagBold . Str
      space = Str " "
      bold = TagBold . Str

