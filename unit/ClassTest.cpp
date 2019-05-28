#include "TestBase.hpp"

#include "util/DebugTrace.hpp"

using namespace std;
using namespace scam;

class ClassTest : public TestBase
{
};

TEST_F(ClassTest, ClassMaker)
{
    ScamValue expr = parseAndEvaluate("make-class");
    expectApplicable(expr, "Special Form make-class", true);
}

TEST_F(ClassTest, TrivialClass)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/trivial.scm");
    expectClass(expr);
}

TEST_F(ClassTest, TrivialInstance)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/trivial2.scm");
    expectInstance(expr);
}

TEST_F(ClassTest, SimpleConstantClass)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/simpleconstant.scm");
    expectInteger(expr, 42, "42", true);
}

TEST_F(ClassTest, ClassWithComputation)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/nontrivial.scm");
    expectList(expr, "(0 42)", 2);
}

TEST_F(ClassTest, InitError)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/initerror.scm");
    expectError(expr);
}

TEST_F(ClassTest, NoInitFunction)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/noinit.scm");
    expectNil(expr);
}

TEST_F(ClassTest, CallMemberFunction)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/callmember.scm");
    expectInteger(expr, 17, "17", true);
}

TEST_F(ClassTest, CallParentFunction)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/callparent.scm");
    expectInteger(expr, 17, "17", true);
}

TEST_F(ClassTest, CallSelfFunction)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/callself.scm");
    expectInteger(expr, 34, "34", true);
}

TEST_F(ClassTest, DirectToBase)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/callbase.scm");
    expectInteger(expr, -1, "-1", true);
}

TEST_F(ClassTest, FunctionInvocationEnv)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/invokeenv.scm");
    expectList(expr, "(2 3 5)", 3);
}

TEST_F(ClassTest, BadFunctionForm)
{
    ScamValue expr = parseAndEvaluateFile("scripts/class/badfunctionform.scm");
    expectError(expr);
}
