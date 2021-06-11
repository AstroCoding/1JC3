import qualified Assign_2             as A2
import qualified Assign_2_ExtraCredit as A2E
import           Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Initializing Test Suite Environment."
  putStrLn "Commencing Test Protocols for A2:"

  putStrLn "Testing gaussConj Test Cases:"
  print (A2.gaussConj (1, 3) == (1, -3))
  print (A2.gaussConj (7, -8) == (7, 8))
  print (A2.gaussConj (4, -(-2)) == (4, -2))

  putStrLn "Testing gaussAdd Test Cases:"
  print (A2.gaussAdd (2, 5) (9, -2) == (11, 3))
  print (A2.gaussAdd (-3, -5) (-9, -2) == (-12, -7))
  print (A2.gaussAdd (3, 5) (-9, -2) == (-6, 3))

  putStrLn "Testing gaussMult Test Cases:"
  print (A2.gaussMult (3,7) (2,-5) == (41,-1))
  print (A2.gaussMult (4,3) (4,-3) == (25,0))
  print (A2.gaussMult (4,0) (0,-3) == (0,-12))

  putStrLn "Testing gaussNorm Test Cases:"
  print (A2.gaussNorm (4,3) == 25)
  print (A2.gaussNorm (0,-12) == 144)
  print (A2.gaussNorm (0,0) == 0)

  putStrLn "Testing maxGaussNorm Test Cases:"
  print (A2.maxGaussNorm [(1,1),(4,3),(2,2)] == (4,3))
  print (A2.maxGaussNorm [(2,1),(7,8),(1,3),(8,7)] == (7,8))
  print (A2.maxGaussNorm [(2,1),(0,0),(3,3)] == (3,3))

  putStrLn "A2 Test Protocols Complete:"
  if A2.gaussConj (1, 3) == (1, -3) && A2.gaussConj (7, -8) == (7, 8) && A2.gaussConj (4, -(-2)) == (4, -2) && A2.gaussAdd (2, 5) (9, -2) == (11, 3) && A2.gaussAdd (-3, -5) (-9, -2) == (-12, -7) && A2.gaussAdd (3, 5) (-9, -2) == (-6, 3) && A2.gaussMult (3,7) (2,-5) == (41,-1) && A2.gaussMult (4,3) (4,-3) == (25,0) && A2.gaussMult (4,0) (0,-3) == (0,-12) && A2.gaussNorm (4,3) == 25 && A2.gaussNorm (0,-12) == 144 && A2.gaussNorm (0,0) == 0 && A2.maxGaussNorm [(1,1),(4,3),(2,2)] == (4,3) && A2.maxGaussNorm [(2,1),(7,8),(1,3),(8,7)] == (7,8) && A2.maxGaussNorm [(2,1),(0,0),(3,3)] == (3,3)
    then putStrLn "All tests successful."
    else putStrLn "A test has failed."
