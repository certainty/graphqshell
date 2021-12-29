{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Infrastructure.TuiEngine.DataGraph where

import qualified Data.HashMap.Strict as Map
import Relude

newtype Keyword a = KV {unKeyword :: a}
  deriving (Eq, Show, Ord, Hashable)

class ToKeyword a b where
  toKeyword :: a -> Keyword b

data Transaction kv = Ls [Transaction kv] | Prop (Keyword kv) | Join (Transaction kv) (Transaction kv) | Ident (Keyword kv) (Transaction kv) | Id Int deriving (Eq, Show)

-- empty

empty :: Transaction Text
empty = Ls []

-- properties
properties :: Transaction Text
properties = Ls [Prop (KV "album/name"), Prop (KV "album/year")]

-- joins
joins :: Transaction Text
joins =
  Ls
    [ Join
        (Prop (KV "favorite-albums"))
        (Ls [Prop (KV "album/name"), Prop (KV "album/year")])
    ]

-- nested joins
nestedJoins :: Transaction Text
nestedJoins =
  Ls
    [ Join
        (Prop (KV "favorite-albums"))
        ( Ls
            [ Prop (KV "album/name"),
              Prop (KV "album/year"),
              Join
                (Prop (KV "album-tracks"))
                (Ls [Prop (KV "track/name"), Prop (KV "track/duration")])
            ]
        )
    ]

-- idents
idents :: Transaction Text
idents = Ident (KV "album/name") (Id 1)

data Property kv value = Value value | Pointer (Keyword kv) TableId deriving (Eq, Show)

data Entity kv value = Entity
  { _entityId :: Int,
    _entityProperties :: Map.HashMap (Keyword kv) (Property kv value)
  }
  deriving (Eq, Show)

newtype TableId = TableId Int deriving (Eq, Show, Hashable)

data Table kv value = Table
  { _tableName :: Keyword kv,
    _rows :: Map.HashMap TableId (Entity kv value)
  }

data Database kv value = Database
  { _tables :: Map.HashMap (Keyword kv) (Table kv value)
  }

--
-- ;; TABLE     ID   ENTITY                                           POINTER TO ADDRESS TABLE
-- { :PERSON { 1    {:person/id 1 :person/name "Joe" :person/address [:ADDRESS 42]}}
--  :ADDRESS { 42   {:address/id 42 :address/street "111 Main St."}}}
--

personTable :: Table Text Text
personTable =
  Table
    (KV "person")
    ( Map.fromList
        [ ( TableId 1,
            Entity
              1
              ( Map.fromList
                  [ (KV "person/id", Value "1"),
                    (KV "person/name", Value "Joe"),
                    (KV "person/address", Pointer (KV "address") (TableId 42))
                  ]
              )
          )
        ]
    )

addressTable :: Table Text Text
addressTable =
  Table
    (KV "address")
    ( Map.fromList
        [ ( TableId 42,
            Entity
              42
              (Map.fromList [(KV "address/id", Value "1"), (KV "address/street", Value "111 Main St.")])
          )
        ]
    )

database :: Database Text Text
database =
  Database
    ( Map.fromList
        [ ( KV "person",
            personTable
          ),
          ( KV "address",
            addressTable
          )
        ]
    )

--- query

-- query :: Transaction kv -> Database kv value -> [Entity kv value]
-- query (Ls transactions) db = concatMap (queryTable transactions db) (Map.elems (db ^. tables))

-- queryTable :: [Transaction kv] -> Database kv value -> a0 -> [Entity kv value]
-- queryTable = error "not implemented"
