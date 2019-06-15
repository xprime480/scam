#include "TestBase.hpp"

#include "util/DebugTrace.hpp"

using namespace std;
using namespace scam;

class ClassTest : public TestBase
{
};

TEST_F(ClassTest, ClassMaker)
{
    ScamValue expr = readEval("make-class");
    expectApplicable(expr, "Special Form make-class", true);
}

TEST_F(ClassTest, TrivialClass)
{
    ScamValue expr = readEvalFile("scripts/class/trivial.scm");
    expectClass(expr);
}

TEST_F(ClassTest, TrivialInstance)
{
    ScamValue expr = readEvalFile("scripts/class/trivial2.scm");
    expectInstance(expr);
}

TEST_F(ClassTest, SimpleConstantClass)
{
    ScamValue expr = readEvalFile("scripts/class/simpleconstant.scm");
    expectInteger(expr, 42, "42", true);
}

TEST_F(ClassTest, ClassWithComputation)
{
    ScamValue expr = readEvalFile("scripts/class/nontrivial.scm");
    expectList(expr, "(0 42)", 2);
}

TEST_F(ClassTest, InitError)
{
    ScamValue expr = readEvalFile("scripts/class/initerror.scm");
    expectError(expr);
}

TEST_F(ClassTest, NoInitFunction)
{
    ScamValue expr = readEvalFile("scripts/class/noinit.scm");
    expectNull(expr);
}

TEST_F(ClassTest, CallMemberFunction)
{
    ScamValue expr = readEvalFile("scripts/class/callmember.scm");
    expectInteger(expr, 17, "17", true);
}

TEST_F(ClassTest, CallParentFunction)
{
    ScamValue expr = readEvalFile("scripts/class/callparent.scm");
    expectInteger(expr, 17, "17", true);
}

TEST_F(ClassTest, CallSelfFunction)
{
    ScamValue expr = readEvalFile("scripts/class/callself.scm");
    expectInteger(expr, 34, "34", true);
}

TEST_F(ClassTest, DirectToBase)
{
    ScamValue expr = readEvalFile("scripts/class/callbase.scm");
    expectInteger(expr, -1, "-1", true);
}

TEST_F(ClassTest, FunctionInvocationEnv)
{
    ScamValue expr = readEvalFile("scripts/class/invokeenv.scm");
    expectList(expr, "(2 3 5)", 3);
}

TEST_F(ClassTest, BadFunctionForm)
{
    ScamValue expr = readEvalFile("scripts/class/badfunctionform.scm");
    expectError(expr);
}
