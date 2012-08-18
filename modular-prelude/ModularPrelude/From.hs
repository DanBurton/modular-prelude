{-# LANGUAGE NoImplicitPrelude #-}

-- | Here you will find the polymorphic values
-- which represent all of the interfaces
-- that a given module can fulfill.
-- You can tell which module a given polymorphic value represents
-- by removing the first and last underscores,
-- and replacing all other underscores with dots.
-- For example, @_Data_ByteString_@ represents the @Data.ByteString@ module.
-- 
-- If you'd like to add a new instance for a given interface,
-- then you must import that module's corresponding typeclass
-- (those are not exported here).
module ModularPrelude.From
  ( X._Data_ByteString_
  , X._Data_ByteString_Lazy_
  , X._Data_Text_
  , X._Data_Text_Lazy_
  , X._Data_Vector_
  , X._Data_Vector_Unboxed_
  , X._Data_Map_
  , X._Data_HashMap_Strict_
  , X._Data_HashMap_Lazy_
  , X._Data_List_
  , X._Data_Set_
  , X._Data_HashSet_
  , X._Filesystem_Path_CurrentOS_
  ) where

import ModularPrelude.Module.ByteString  as X (_Data_ByteString_)
import ModularPrelude.Module.LByteString as X (_Data_ByteString_Lazy_)
import ModularPrelude.Module.Text        as X (_Data_Text_)
import ModularPrelude.Module.LText       as X (_Data_Text_Lazy_)
import ModularPrelude.Module.Vector      as X (_Data_Vector_)
import ModularPrelude.Module.UVector     as X (_Data_Vector_Unboxed_)
import ModularPrelude.Module.Map         as X (_Data_Map_)
import ModularPrelude.Module.HashMap     as X (_Data_HashMap_Strict_)
import ModularPrelude.Module.LHashMap    as X (_Data_HashMap_Lazy_)
import ModularPrelude.Module.List        as X (_Data_List_)
import ModularPrelude.Module.Set         as X (_Data_Set_)
import ModularPrelude.Module.HashSet     as X (_Data_HashSet_)
import ModularPrelude.Module.FilePath    as X (_Filesystem_Path_CurrentOS_)

