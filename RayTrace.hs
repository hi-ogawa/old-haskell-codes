{-# LANGUAGE TemplateHaskell #-}
-- start at 1/25 16:06 ...

module RayTrace where

-- vectors type and its utilities
import Data.Vect

{-
v0  = Vec3 1 2 3 :: Vec3  -- define a vector
v1  = 5 *& v0             -- scalar multiplication
v2  = v0 &+ v1            -- vector addition
v2' = v0 &- v1            -- vector substraction
l0  = norm v2             -- norm
v4  = normalize v2        -- normalize
v5  = neg v2              -- negation
-}

data Sphere = Sphere { ce  :: Vec3,
                       ra  :: Vec3,
                       ra2 :: Vec3,
                       sC  :: Vec3,
                       eC  :: Vec3,
                       transp  :: Float,
                       reflect :: Float
                     }

intersect :: Vec3 -> Vec3 -> Float -> Float -> Bool
intersect rayorig raydir t0 t1 = undefined

