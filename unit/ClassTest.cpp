
#include "TestBase.hpp"

using namespace std;
using namespace scam;

class ClassTest : public TestBase
{
};

TEST_F(ClassTest, ClassMaker)
{
    ScamExpr * expr = parseAndEvaluate("make-class");
    expectApplicable(expr, "Special Form class-maker");
}

TEST_F(ClassTest, TrivialClass)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/trivial.scm");
    expectClass(expr);
}

TEST_F(ClassTest, TrivialInstance)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/trivial2.scm");
    expectInstance(expr);
}

TEST_F(ClassTest, SimpleConstantClass)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/simpleconstant.scm");
    expectInteger(expr, 42, "42");
}

TEST_F(ClassTest, ClassWithComputation)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/nontrivial.scm");
    expectList(expr, "(0 42)", 2);
}

TEST_F(ClassTest, InitError)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/initerror.scm");
    expectError(expr);
}

TEST_F(ClassTest, NoInitFunction)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/noinit.scm");
    expectNil(expr);
}

TEST_F(ClassTest, CallMemberFunction)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/callmember.scm");
    expectInteger(expr, 17, "17");
}

TEST_F(ClassTest, CallParentFunction)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/callparent.scm");
    expectInteger(expr, 17, "17");
}

TEST_F(ClassTest, CallSelfFunction)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/callself.scm");
    expectInteger(expr, 34, "34");
}

TEST_F(ClassTest, DirectToBase)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/callbase.scm");
    expectInteger(expr, -1, "-1");
}

TEST_F(ClassTest, FunctionInvocationEnv)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/class/invokeenv.scm");
    expectList(expr, "(2 3 5)", 3);
}
