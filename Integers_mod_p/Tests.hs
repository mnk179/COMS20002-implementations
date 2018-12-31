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

addInvTest1 = TestCase (assertEqual "for 2 (mod 5) (add inv)" (addInvModP (IntModP 2 5)) (IntModP 3 5))
addInvTest2 = TestCase (assertEqual "for 3 (mod 4) (add inv)" (addInvModP (IntModP 3 4)) (IntModP 1 4))
addInvTest3 = TestCase (assertEqual "for 1 (mod 4) (add inv)" (addInvModP (IntModP 1 4)) (IntModP 3 4))

addInvTests = TestList [TestLabel "Additive Inverse Test 1" addInvTest1,
                    TestLabel "Additive Inverse Test 2" addInvTest2,
                    TestLabel "Additive Inverse Test 3" addInvTest3]

mulTest1 = TestCase (assertEqual "for 2 * 3 (mod 5)" (mulModP (IntModP 2 5) (IntModP 3 5)) (IntModP 1 5))
mulTest2 = TestCase (assertEqual "for 3 * 2 (mod 4)" (mulModP (IntModP 3 4) (IntModP 2 4)) (IntModP 2 4))
mulTest3 = TestCase (assertEqual "for 1 * 2 (mod 4)" (mulModP (IntModP 1 4) (IntModP 2 4)) (IntModP 2 4))

mulTests = TestList [TestLabel "Multiplication Test 1" mulTest1,
                    TestLabel "Multiplication Test 2" mulTest2,
                    TestLabel "Multiplication Test 3" mulTest3]

mulInvTest1 = TestCase (assertEqual "for 2 (mod 5) (mul inv)" (mulInvModP (IntModP 2 5)) (IntModP 3 5))
mulInvTest2 = TestCase (assertEqual "for 3 (mod 7) (mul inv)" (mulInvModP (IntModP 3 7)) (IntModP 5 7))
mulInvTest3 = TestCase (assertEqual "for 1 (mod 11) (mul inv)" (mulInvModP (IntModP 1 11)) (IntModP 1 11))
mulInvTest4 = TestCase (assertEqual "for 7 (mod 11) (mul inv)" (mulInvModP (IntModP 7 11)) (IntModP 8 11))

mulInvTests = TestList [TestLabel "Multiplicative Inverse Test 1" mulInvTest1,
                    TestLabel "Multiplicative Inverse Test 2" mulInvTest2,
                    TestLabel "Multiplicative Inverse Test 3" mulInvTest3,
                    TestLabel "Multiplicative INverse Test 4" mulInvTest4]

divTest1 = TestCase (assertEqual "for 2 / 3 (mod 5)" (divModP (IntModP 2 5) (IntModP 3 5)) (IntModP 4 5))
divTest2 = TestCase (assertEqual "for 3 / 2 (mod 7)" (divModP (IntModP 3 7) (IntModP 2 7)) (IntModP 5 7))
divTest3 = TestCase (assertEqual "for 1 / 2 (mod 11)" (divModP (IntModP 1 11) (IntModP 2 11)) (IntModP 6 11))
divTest4 = TestCase (assertEqual "for 5 / 6 (mod 13)" (divModP (IntModP 5 13) (IntModP 6 13)) (IntModP 3 13))

divTests = TestList [TestLabel "Division Test 1" divTest1,
                    TestLabel "Division Test 2" divTest2,
                    TestLabel "Division Test 3" divTest3,
                    TestLabel "Division Test 4" divTest4]

expTest1 = TestCase (assertEqual "for 2^5 (mod 5)" (expModP (IntModP 2 5) 5) (IntModP 2 5))
expTest2 = TestCase (assertEqual "for 3^2 (mod 7)" (expModP (IntModP 3 7) 2) (IntModP 2 7))
expTest3 = TestCase (assertEqual "for 1^11 (mod 11)" (expModP (IntModP 1 11) 11) (IntModP 1 11))
expTest4 = TestCase (assertEqual "for 5^219 (mod 13)" (expModP (IntModP 5 13) 219) (IntModP 8 13))

expTests = TestList [TestLabel "Exponentiation Test 1" expTest1,
                    TestLabel "Exponentiation Test 2" expTest2,
                    TestLabel "Exponentiation Test 3" expTest3,
                    TestLabel "Exponentiation Test 4" expTest4]

bigTest1 = TestCase (assertEqual "for 999999 + 999999 (mod 104831)" (addModP (IntModP 999999 104831) (IntModP 999999 104831)) (IntModP 8209 104831))
bigTest2 = TestCase (assertEqual "for 201042 - 421888 (mod 105517)" (subModP (IntModP 201042 105517) (IntModP 421888 105517)) (IntModP 95705 105517))
bigTest3 = TestCase (assertEqual "for 294251 (mod 1300963) (add inv)" (addInvModP (IntModP 294251 1300963)) (IntModP 1006712 1300963))
bigTest4 = TestCase (assertEqual "for 103910 * 77201 (mod 1301021)" (mulModP (IntModP 103910 1301021) (IntModP 77201 1301021)) (IntModP 1161445 1301021))
bigTest5 = TestCase (assertEqual "for 9909120 (mod 23879413) (mul inv)" (mulInvModP (IntModP 9909120 23879413)) (IntModP 7876913 23879413))
bigTest6 = TestCase (assertEqual "for 5041094 / 2124152 (mod 23879123)" (divModP (IntModP 5041094 23879123) (IntModP 2124152 23879123)) (IntModP 13481141 23879123))
bigTest7 = TestCase (assertEqual "for 2013010^999 (mod 15485737)" (expModP (IntModP 2013010 15485737) 999) (IntModP 5728802 15485737))

bigTests = TestList [TestLabel "Big Number Test 1" bigTest1,
                    TestLabel "Big Number Test 2" bigTest2,
                    TestLabel "Big Number Test 3" bigTest3,
                    TestLabel "Big Number Test 4" bigTest4,
                    TestLabel "Big Number Test 5" bigTest5,
                    TestLabel "Big Number Test 6" bigTest6,
                    TestLabel "Big Number Test 7" bigTest7]

tests = TestList [addTests, subTests, addInvTests, mulTests, mulInvTests, divTests, expTests, bigTests]