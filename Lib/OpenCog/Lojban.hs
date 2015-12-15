module OpenCog.Lojban
( module OpenCog.Lojban
, module OpenCog.Lojban.Util
, lojbanToAtomese
) where

import OpenCog.Lojban.Util
import OpenCog.Lojban.SuReal
import OpenCog.Lojban.Parser
import OpenCog.AtomSpace
import Foreign.C
import Foreign.Ptr

foreign export ccall "lojbanToAtomese"
    c_lojbanToAtomese :: Ptr AtomSpaceRef -> UUID -> IO (UUID)

c_lojbanToAtomese = exportFunction lojbanToAtomese

foreign export ccall "atomeseToLojban"
    c_atomeseToLojban :: Ptr AtomSpaceRef -> UUID -> IO (UUID)

c_atomeseToLojban = exportFunction atomeseToLojban


