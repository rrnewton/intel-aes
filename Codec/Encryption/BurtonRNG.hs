
-- | This version uses an Intel AES-NI based assembly code
-- | implementation of encryption.  Or, on processors where AES-NI is
-- | unavailable, it falls back to Dr. Brian Gladman's implementation.
-- | This module is simply a wrapper around the Intel-provided AESNI
-- | sample library:
-- |    http://software.intel.com/en-us/articles/download-the-intel-aesni-sample-library/

module Codec.Encryption.BurtonRNGSlow     
    (
     mkBurtonGen
     -- Plus, instances exported of course.
    )
where 

import System.Random ()
import Crypto.Random.DRBG ()



-- UNFINISHED
