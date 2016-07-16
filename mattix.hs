import System.Random (getStdGen)
import System.IO.Unsafe (unsafePerformIO)

board = newArray ((0,0), (5,5)) 0