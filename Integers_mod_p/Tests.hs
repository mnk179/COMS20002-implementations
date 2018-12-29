import Test.HUnit
import Integers

addTest1 = TestCase (assertEqual "for 2 + 3 (mod 5)" (addModP (IntModP 2 5) (IntModP 3 5)) (IntModP 0 5))
addTest2 = TestCase (assertEqual "for 3 + 2 (mod 4)" (addModP (IntModP 3 4) (IntModP 2 4)) (IntModP 1 4))
addTest3 = TestCase (assertEqual "for 1 + 2 (mod 4)" (addModP (IntModP 1 4) (IntModP 2 4)) (IntModP 3 4))

addTests = TestList [TestLabel "Addition Test 1" addTest1,
                    TestLabel "Addition Test 2" addTest2,
                    TestLabel "Addition Test 3" addTest3]