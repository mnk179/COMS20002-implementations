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

addInvTest1 = TestCase (assertEqual "for 2 (mod 5)" (addInvModP (IntModP 2 5)) (IntModP 3 5))
addInvTest2 = TestCase (assertEqual "for 3 (mod 4)" (addInvModP (IntModP 3 4)) (IntModP 1 4))
addInvTest3 = TestCase (assertEqual "for 1 (mod 4)" (addInvModP (IntModP 1 4)) (IntModP 3 4))

addInvTests = TestList [TestLabel "Additive Inverse Test 1" addInvTest1,
                    TestLabel "Additive Inverse Test 2" addInvTest2,
                    TestLabel "Additive Inverse Test 3" addInvTest3]

mulTest1 = TestCase (assertEqual "for 2 * 3 (mod 5)" (mulModP (IntModP 2 5) (IntModP 3 5)) (IntModP 1 5))
mulTest2 = TestCase (assertEqual "for 3 * 2 (mod 4)" (mulModP (IntModP 3 4) (IntModP 2 4)) (IntModP 2 4))
mulTest3 = TestCase (assertEqual "for 1 * 2 (mod 4)" (mulModP (IntModP 1 4) (IntModP 2 4)) (IntModP 2 4))

mulTests = TestList [TestLabel "Multiplication Test 1" mulTest1,
                    TestLabel "Multiplication Test 2" mulTest2,
                    TestLabel "Multiplication Test 3" mulTest3]

mulInvTest1 = TestCase (assertEqual "for 2 (mod 5)" (mulInvModP (IntModP 2 5)) (IntModP 3 5))
mulInvTest2 = TestCase (assertEqual "for 3 (mod 4)" (mulInvModP (IntModP 3 7)) (IntModP 5 7))
mulInvTest3 = TestCase (assertEqual "for 1 (mod 4)" (mulInvModP (IntModP 1 11)) (IntModP 1 11))
mulInvTest4 = TestCase (assertEqual "for 1 (mod 4)" (mulInvModP (IntModP 7 11)) (IntModP 8 11))

mulInvTests = TestList [TestLabel "Multiplicative Inverse Test 1" mulInvTest1,
                    TestLabel "Multiplicative Inverse Test 2" mulInvTest2,
                    TestLabel "Multiplicative Inverse Test 3" mulInvTest3,
                    TestLabel "Multiplicative INverse Test 4" mulInvTest4]

tests = TestList [addTests, subTests, addInvTests, mulTests, mulInvTests]