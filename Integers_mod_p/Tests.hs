import Test.HUnit
import Integers

addTest1 = TestCase (assertEqual "for 2 + 3 (mod 5)" (addModP (IntModP 2 5) (IntModP 3 5)) (IntModP 0 5))
addTest2 = TestCase (assertEqual "for 3 + 2 (mod 4)" (addModP (IntModP 3 4) (IntModP 2 4)) (IntModP 1 4))
addTest3 = TestCase (assertEqual "for 1 + 2 (mod 4)" (addModP (IntModP 1 4) (IntModP 2 4)) (IntModP 3 4))

addTests = TestList [TestLabel "Addition Test 1" addTest1,
                    TestLabel "Addition Test 2" addTest2,
                    TestLabel "Addition Test 3" addTest3]

subTest1 = TestCase (assertEqual "for 2 - 3 (mod 5)" (subModP (IntModP 2 5) (IntModP 3 5)) (IntModP 4 5))
subTest2 = TestCase (assertEqual "for 3 - 2 (mod 4)" (subModP (IntModP 3 4) (IntModP 2 4)) (IntModP 1 4))
subTest3 = TestCase (assertEqual "for 1 - 2 (mod 4)" (subModP (IntModP 1 4) (IntModP 2 4)) (IntModP 3 4))

subTests = TestList [TestLabel "Subtraction Test 1" subTest1,
                    TestLabel "Subtraction Test 2" subTest2,
                    TestLabel "Subtraction Test 3" subTest3]

addInvModPTest1 = TestCase (assertEqual "for 2 (mod 5)" (addInvModP (IntModP 2 5)) (IntModP 3 5))
addInvModPTest2 = TestCase (assertEqual "for 3 (mod 4)" (addInvModP (IntModP 3 4)) (IntModP 1 4))
addInvModPTest3 = TestCase (assertEqual "for 1 (mod 4)" (addInvModP (IntModP 1 4)) (IntModP 3 4))

addInvModPTests = TestList [TestLabel "Additive Inverse Test 1" addInvModPTest1,
                    TestLabel "Additive Inverse Test 2" addInvModPTest2,
                    TestLabel "Additive Inverse Test 3" addInvModPTest3]

tests = TestList [addTests, subTests, addInvModPTests]