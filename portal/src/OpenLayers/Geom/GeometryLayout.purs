-- |
-- | The OpenLayers GeometryLayout module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geom.GeometryLayout (
    GeometryLayout
    , xy
    , xyz
    , xym
    , xyzm ) where

--
-- Type definitions for gemoetry layout
--
newtype GeometryLayout = GeometryLayout String

foreign import xy::GeometryLayout
foreign import xyz::GeometryLayout
foreign import xym::GeometryLayout
foreign import xyzm::GeometryLayout
