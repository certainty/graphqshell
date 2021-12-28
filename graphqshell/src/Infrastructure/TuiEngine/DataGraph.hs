{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Infrastructure.TuiEngine.DataGraph where

import qualified Data.HashMap.Strict as Map
import Infrastructure.TuiEngine.Keys (Key (Key))
import Relude
import Relude (Hashable)

--
-- { :foo/bar [:foo/baz :foo/qux] }
--
-- Let's star to shape the API from the usage side
--
-- properties
-- query #_ ["foo/baz"]

-- query ident
-- `query [("foo/bar" 123)]

type Keyword = Text

data Transaction = Ls [Transaction] | Prop Keyword | Join Transaction Transaction | Ident Keyword Transaction | Id Int deriving (Eq, Show)

-- empty

empty :: Transaction
empty = Ls []

-- properties
properties :: Transaction
properties = Ls [Prop "album/name", Prop "album/year"]

-- joins
joins :: Transaction
joins =
  Ls
    [ Join
        (Prop "favorite-albums")
        (Ls [Prop "album/name", Prop "album/year"])
    ]

-- nested joins
nestedJoins :: Transaction
nestedJoins =
  Ls
    [ Join
        (Prop "favorite-albums")
        ( Ls
            [ Prop "album/name",
              Prop "album/year",
              Join
                (Prop "album-tracks")
                (Ls [Prop "track/name", Prop "track/duration"])
            ]
        )
    ]

-- idents
idents :: Transaction
idents = Ident "album/name" (Id 1)

data Property value = Value value | Pointer Keyword TableId deriving (Eq, Show)

data Entity value = Entity
  { _entityId :: Int,
    _entityProperties :: Map.HashMap Keyword (Property value)
  }
  deriving (Eq, Show)

newtype TableId = TableId Int deriving (Eq, Show, Hashable)

data Table value = Table
  { _tableName :: Keyword,
    _rows :: Map.HashMap TableId (Entity value)
  }

data Database value = Database
  { _tables :: Map.HashMap Keyword (Table value)
  }

--
-- ;; TABLE     ID   ENTITY                                           POINTER TO ADDRESS TABLE
-- { :PERSON { 1    {:person/id 1 :person/name "Joe" :person/address [:ADDRESS 42]}}
--  :ADDRESS { 42   {:address/id 42 :address/street "111 Main St."}}}
--

personTable :: Table Text
personTable =
  Table
    "person"
    ( Map.fromList
        [ ( TableId 1,
            Entity
              1
              ( Map.fromList
                  [ ("person/id", Value "1"),
                    ("person/name", Value "Joe"),
                    ("person/address", Pointer "address" (TableId 42))
                  ]
              )
          )
        ]
    )

addressTable :: Table Text
addressTable =
  Table
    "address"
    ( Map.fromList
        [ ( TableId 42,
            Entity
              42
              (Map.fromList [("address/id", Value "1"), ("address/street", Value "111 Main St.")])
          )
        ]
    )

database :: Database Text
database =
  Database
    ( Map.fromList
        [ ( "person",
            personTable
          ),
          ( "address",
            addressTable
          )
        ]
    )
