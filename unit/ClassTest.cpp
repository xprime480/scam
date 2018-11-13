
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class ClassTest : public ExpressionTestBase
{
};

TEST_F(ClassTest, ClassMaker)
{
    ExprHandle expr = parseAndEvaluate("make-class");
    expectApplicable(expr, "Special Form class-maker");
}

TEST_F(ClassTest, TrivialClass)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/class/trivial.scm");
    expectClass(expr);
}

TEST_F(ClassTest, TrivialInstance)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/class/trivial2.scm");
    expectInstance(expr);
}

TEST_F(ClassTest, SimpleConstantClass)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/class/simpleconstant.scm");
    expectInteger(expr, 42, "42");
}

TEST_F(ClassTest, ClassWithComputation)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/class/nontrivial.scm");
    expectList(expr, "(0 42)", 2);
}

TEST_F(ClassTest, InitError)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/class/initerror.scm");
    expectError(expr);
}

TEST_F(ClassTest, NoInitFunction)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/class/noinit.scm");
    expectNil(expr);
}

TEST_F(ClassTest, CallMemberFunction)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/class/callmember.scm");
    expectInteger(expr, 17, "17");
}

TEST_F(ClassTest, CallParentFunction)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/class/callparent.scm");
    expectInteger(expr, 17, "17");
}

TEST_F(ClassTest, CallSelfFunction)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/class/callself.scm");
    expectInteger(expr, 34, "34");
}

TEST_F(ClassTest, DirectToBase)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/class/callbase.scm");
    expectInteger(expr, -1, "-1");
}
