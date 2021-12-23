{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Infrastructure.TuiEngine.DataGraph where

import qualified Data.HashMap.Strict as Map
import Relude

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
