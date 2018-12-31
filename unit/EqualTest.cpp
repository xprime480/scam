
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class EqualTest : public ExpressionTestBase
{
};

TEST_F(EqualTest, EqPEmpty)
{
    expectTrue("(eq?)");
}

TEST_F(EqualTest, EqPSingleBoolTrue)
{
    expectTrue("(eq? #t)");
}

TEST_F(EqualTest, EqPSingleBoolFalse)
{
    expectTrue("(eq? #f)");
}

TEST_F(EqualTest, EqPSingleInteger)
{
    expectTrue("(eq? 1)");
}

TEST_F(EqualTest, EqPSingleFloat)
{
    expectTrue("(eq? 3.14159)");
}

TEST_F(EqualTest, EqPSingleString)
{
    expectTrue("(eq? \"abc\")");
}

TEST_F(EqualTest, EqPSingleCons)
{
    expectTrue("(eq? (cons 'x 3))");
}

TEST_F(EqualTest, EqPSingleList)
{
    expectTrue("(eq? (list 1 2 3))");
}

TEST_F(EqualTest, EqPSingleChar)
{
    expectTrue("(eq? #\\a)");
}

TEST_F(EqualTest, EqPSingleVector)
{
    expectTrue("(eq? [1 2 3])");
}

TEST_F(EqualTest, EqPSingleSymbol)
{
    expectTrue("(eq? 'a-symbol)");
}

TEST_F(EqualTest, EqPSingleClass)
{
    expectTrue("(eq? (make-class Root () (init ())))");
}

TEST_F(EqualTest, EqPSingleInstance)
{
    expectTrue("(eq? ((make-class Root () (init ()))))");
}

TEST_F(EqualTest, EqPSingleProc)
{
    expectTrue("(eq? (lambda (x) (+ 1 x)))");
}

TEST_F(EqualTest, EqPTwoBoolSameTrue)
{
    expectTrue("(eq? #t #t)");
}

TEST_F(EqualTest, EqPTwoBoolSameFalse)
{
    expectTrue("(eq? #f #f)");
}

TEST_F(EqualTest, EqPTwoBoolDifferent)
{
    expectFalse("(eq? #t #f)");
}

TEST_F(EqualTest, EqPTwoIntegerSame)
{
    expectTrue("(eq? 1 1)");
}

TEST_F(EqualTest, EqPTwoIntegerDifferent)
{
    expectFalse("(eq? 1 2)");
}

TEST_F(EqualTest, EqPTwoFloatSame)
{
    expectTrue("(eq? 3.14159 3.14159)");
}

TEST_F(EqualTest, EqPTwoFloatDifferent)
{
    expectFalse("(eq? 3.14159 -123.456)");
}

TEST_F(EqualTest, EqPMixedNumeric)
{
    expectTrue("(eq? 1 1.0)");
}

TEST_F(EqualTest, EqPTwoStringSame)
{
    expectTrue("(eq? \"abc\" \"abc\")");
}

TEST_F(EqualTest, EqPTwoStringDifferent)
{
    expectFalse("(eq? \"abc\" \"stu\")");
}

TEST_F(EqualTest, EqPTwoConsSame)
{
    expectTrue("(eq? (cons 'x 3) (cons 'x 3))");
}

TEST_F(EqualTest, EqPTwoConsDifferent)
{
    expectFalse("(eq? (cons 'x 3) (cons 'z 9))");
}

TEST_F(EqualTest, EqPTwoListSame)
{
    expectTrue("(eq? (list 1 2 3) (list 1 2 3))");
}

TEST_F(EqualTest, EqPTwoListDifferent)
{
    expectFalse("(eq? (list 1 2 3) (list 8 8 8))");
}

TEST_F(EqualTest, EqPTwoListDifferentSize)
{
    expectFalse("(eq? (list 1 2) (list 1 2 3))");
}

TEST_F(EqualTest, EqPTwoCharSame)
{
    expectTrue("(eq? #\\a #\\a)");
}

TEST_F(EqualTest, EqPTwoCharDifferent)
{
    expectFalse("(eq? #\\a #\\?)");
}

TEST_F(EqualTest, EqPTwoVectorSame)
{
    expectTrue("(eq? [1 2 3] [1 2 3])");
}

TEST_F(EqualTest, EqPTwoVectorDifferent)
{
    expectFalse("(eq? [1 2 3] [1 2 3 4 5 6 7 8])");
}

TEST_F(EqualTest, EqPTwoVectorOneEmpty)
{
    expectFalse("(eq? [] [1 2 3 4 5 6 7 8])");
}

TEST_F(EqualTest, EqPTwoSymbolSame)
{
    expectTrue("(eq? 'a-symbol 'a-symbol)");
}

TEST_F(EqualTest, EqPTwoSymbolDifferent)
{
    expectFalse("(eq? 'a-symbol 'different)");
}

TEST_F(EqualTest, EqPTwoClassSameIdentity)
{
    expectTrue("(load \"scripts/equality/identicalclass.scm\")");
}

TEST_F(EqualTest, EqPTwoClassCopy)
{
    expectFalse("(load \"scripts/equality/samedef.scm\")");
}

TEST_F(EqualTest, EqPTwoClassDifferent)
{
    expectFalse("(load \"scripts/equality/differentclass.scm\")");
}

TEST_F(EqualTest, EqPTwoInstanceSameIdentity)
{
    expectTrue("(load \"scripts/equality/identicalinstance.scm\")");
}

TEST_F(EqualTest, EqPTwoInstanceCopy)
{
    expectFalse("(load \"scripts/equality/samedefinstance.scm\")");
}

TEST_F(EqualTest, EqPTwoInstanceDifferent)
{
    expectFalse("(load \"scripts/equality/differentinstance.scm\")");
}

TEST_F(EqualTest, EqPTwoProcSameIdentity)
{
    expectTrue("(load \"scripts/equality/identicalproc.scm\")");
}

TEST_F(EqualTest, EqPTwoProcDifferent)
{
    expectFalse("(load \"scripts/equality/differentproc.scm\")");
}

TEST_F(EqualTest, EqPKeywordsEqual)
{
    expectTrue("(eq? :a :a)");
}

