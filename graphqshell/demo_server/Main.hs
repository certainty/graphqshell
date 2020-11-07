{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.ByteString.Lazy
import Data.Morpheus.Types (RootResolver (..), Undefined (..))
import Data.Text (Text)
import Data.Morpheus.Kind (INTERFACE)
import Data.Morpheus.Types (GQLType (..), interface)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Data.Morpheus.Types
  ( GQLType,
    ResolverQ,
    RootResolver (..),
    Undefined (..),
    liftEither,
  )

import Data.Morpheus
 ( App,
   deriveApp,
   runApp,
 )

import Web.Scotty
  (
   scotty
  )

import Web.Scotty.Trans
  (
    raw,
    post,
    body
  )
import Control.Monad.IO.Class (MonadIO(liftIO))

data Realm
  = MountOlympus
  | Sky
  | Sea
  | Underworld
  | Dream
  deriving (Generic, GQLType)

data City
  = Athens
  | Colchis
  | Delphi
  | Ithaca
  | Sparta
  | Troy
  deriving (Generic, GQLType)

newtype Person = Person {name :: Text}
  deriving (Generic)

instance GQLType Person where
  type KIND Person = INTERFACE

data Deity = Deity
  { name :: Text, -- Non-Nullable Field
    power :: Maybe Text, -- Nullable Field
    realm :: Realm,
    bornAt :: Maybe City
  }
  deriving (Generic)

instance GQLType Deity where
  implements _ = [interface (Proxy @Person)]

data Human m = Human
  { name :: m Text,
    bornAt :: m City
  }
  deriving (Generic, GQLType)

someHuman :: Applicative m => Human m
someHuman = Human {name = pure "Odysseus", bornAt = pure Ithaca}

someDeity :: Deity
someDeity =
  Deity
    { name = "Morpheus",
      power = Just "Shapeshifting",
      realm = Dream,
      bornAt = Nothing
    }

dbDeity :: Text -> Maybe City -> IO (Either String Deity)
dbDeity _ bornAt =
  return $ Right $
    Deity
      { name = "Morpheus",
        power = Just "Shapeshifting",
        realm = Dream,
        bornAt
      }

data Character m
  = CharacterHuman (Human m) -- Only <tyconName><conName> should generate direct link
  | CharacterDeity Deity -- Only <tyconName><conName> should generate direct link
      -- RECORDS
  | Creature {name :: Text, age :: Int}
  | BoxedDeity {boxedDeity :: Deity}
  | SomeScalarRecord {scalar :: Text}
  | --- Types
    SomeDeity Deity
  | SomeScalar Int
  | SomeMutli Int Text
  | --- ENUMS
    Zeus
  | Cronus
  deriving (Generic, GQLType)

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    character :: [Character m]
  }
  deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name :: Text, -- Required Argument
    bornPlace :: Maybe City -- Optional Argument
  }
  deriving (Generic, GQLType)

resolveDeity :: DeityArgs -> ResolverQ e IO Deity
resolveDeity DeityArgs {name, bornPlace} =
  liftEither $ dbDeity name bornPlace

resolveCharacter :: Applicative m => [Character m]
resolveCharacter =
  [ CharacterHuman someHuman,
    CharacterDeity someDeity,
    Creature {name = "Lamia", age = 205},
    BoxedDeity {boxedDeity = someDeity},
    SomeScalarRecord {scalar = "Some Text"},
    ---
    SomeDeity someDeity,
    SomeScalar 12,
    SomeMutli 21 "some text",
    Zeus,
    Cronus
  ]

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity = resolveDeity, character = resolveCharacter},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

app :: App () IO
app = deriveApp rootResolver

api :: ByteString -> IO ByteString
api = runApp app 

main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . api =<< body)
