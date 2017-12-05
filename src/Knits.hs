module Knits where

-- Fundamental types

-- "Knit" and "purl" connote over/under *with respect to* the working reference
-- frame, which may change over the course of knitting a fabric. Over and Under
-- here are with respect to some fixed initial frame, so Knit and Purl would be
-- misleading names.
data DirectionThru = Over | Under

-- The ref parameter is some type that can be used to refer to previous
-- stitches. The information contained in a Stitch is just which previous
-- stitch[es] (could be multiple, e.g. as a result of k2tog) it is pulled thru,
-- and which direction thru. To geometrically realize the stitch, enter each
-- previous stitch from above or below as appropriate, then exit them in
-- reverse order; note that the order of the list matters!
data Stitch ref = Stitch [(ref, DirectionThru)]

-- The combinatorial data of a piece of fabric. A Fabric is a list of stitches
-- that should be created in sequence from the working yarn, with references to
-- previous stitches in the form of list offsets (not indices! i.e., a ref of 0
-- means the previous stitch, 1 means the one before that, etc).
data Fabric = Fabric [Stitch Int]

