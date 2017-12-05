{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module Knits where

import Data.List

-- Fundamental types

-- "Knit" and "purl" connote over/under *with respect to* the working reference
-- frame, which may change over the course of knitting a fabric. Over and Under
-- here are with respect to some fixed initial frame, so Knit and Purl would be
-- misleading names.
data DirectionThru = Over | Under deriving (Show)

flipDir :: DirectionThru -> DirectionThru
flipDir Over = Under
flipDir Under = Over

-- The ref parameter is some type that can be used to refer to previous
-- stitches. The information contained in a Stitch is just which previous
-- stitch[es] (could be multiple, e.g. as a result of k2tog) it is pulled thru,
-- and which direction thru. To geometrically realize the stitch, enter each
-- previous stitch from above or below as appropriate, then exit them in
-- reverse order; note that the order of the list matters!
data Stitch ref = Stitch [(ref, DirectionThru)]
  deriving (Functor, Foldable, Traversable, Show)

-- The combinatorial data of a piece of fabric. A Fabric is a list of stitches
-- that should be created in sequence from the working yarn, with references to
-- previous stitches in the form of list offsets (not indices! i.e., a ref of 0
-- means the previous stitch, 1 means the one before that, etc).
newtype Offset = Offset {getOffset :: Int} deriving (Eq, Ord, Show)
data Fabric = Fabric {getStitches :: [Stitch Offset]} deriving (Show)


-- Knitting

-- Since some operations (e.g., M1) may retroactively insert new stitches,
-- offset references would require careful updating to point to the right
-- thing. It's more convenient to just use arbitrary fixed stitch labels, then
-- convert to offsets when done.
newtype StitchID = StitchID {getStitchID :: Int} deriving (Eq, Ord, Show)
-- The stitches in a WorkingFabric are stored backwards compared to the
-- stitches in a Fabric; i.e., the most recent stitch is first. That way,
-- making new stitches is just cons instead of snoc.
data WorkingFabric =
  WorkingFabric {getWorkingStitches :: [(StitchID, Stitch StitchID)]}
  deriving (Show)

-- Utility function: Map over a list, but the mapping function gets to see the
-- whole tail.
walk :: (a -> [a] -> b) -> [a] -> [b]
walk f [] = []
walk f (a:as) = f a as : walk f as

-- The Maybe is because we might have nonexistent stitch IDs.
finish :: WorkingFabric -> Maybe Fabric
finish (WorkingFabric sts) = Fabric . reverse <$> sequence (walk reref sts)
  where reref (_, st) old = traverse (findStitch old) st
        findStitch old i = Offset <$> findIndex ((==i) . fst) old

-- Normal knitting is a pair of stacks; circular knitting is a queue. This
-- class lets us be polymorphic over the nature of our needles.
class Needles n where
  popLeft :: n -> Maybe (n, StitchID)
  pushRight :: StitchID -> n -> n
  -- In normal knitting, we flip which side is up when we exhaust the stitches
  -- on the left. To be generic, we'll let the needles value tell us whether
  -- k/p is over/under or under/over.
  sideUp :: n -> DirectionThru

-- Both stitch lists are from top-of-needle to bottom-of-needle.
data TwoNeedles =
  TwoNeedles {
    leftNeedle :: [StitchID],
    rightNeedle :: [StitchID],
    sideUp_ :: DirectionThru}
  deriving (Show)

instance Needles TwoNeedles where
  popLeft (TwoNeedles l r s) = case (l, r) of
    (id:ids, _) -> Just (TwoNeedles ids r s, id)
    -- flip the work when we run out of stitches on the left
    ([], _:_) -> popLeft (TwoNeedles r [] (flipDir s))
    ([], []) -> Nothing

  pushRight id (TwoNeedles l r s) = TwoNeedles l (id:r) s

  sideUp = sideUp_

-- Magic Loop: Actually queue-implemented-with-two-stacks in disguise!
-- Well, more like three stacks in practice.
data CircularNeedles =
  CircularNeedles {
    leftTip :: [StitchID],
    rightTip :: [StitchID]}
  deriving (Show)

instance Needles CircularNeedles where
  popLeft (CircularNeedles l r) = case (l, r) of
    (id:ids, _) -> Just (CircularNeedles ids r, id)
    -- slide the right-hand stitches along the cable when we run out of
    -- stitches on the left
    ([], _:_) -> popLeft (CircularNeedles (reverse r) [])
    ([], []) -> Nothing

  pushRight id (CircularNeedles l r) = CircularNeedles l (id:r)

  sideUp _ = Over

-- TODO: Factor out the common logic for those two.

