import Data.IORef
x = newIORef 0          -- x :t IO (IORef Integer)

incr x = do ioref <- x                  -- ioref :t IORef Integer
            modifyIORef ioref (+1)      -- :t IO ()
            readIORef ioref >>= print
            
incr2 ior = do modifyIORef ior (+1)
               readIORef ior >>= print
  
  
-- main = do incr x
--           incr x
          
main = do ioref <- x
          incr2 ioref
          incr2 ioref