#include "TestBase.hpp"

#include "env/Env.hpp"
#include "form/Library.hpp"
#include "port/FilePort.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;

class LibraryTest : public TestBase
{
protected:
    LibraryTest()
    {
    }

    void SetUp() override
    {
        TestBase::SetUp();

        // FilePort * port = new FilePort("analysis.txt", ScamPort::Writeable);
        // configEnv->put(makeSymbol("**log-port**"), makePort(port));
    }
};

TEST_F(LibraryTest, NestedImport)
{
    ScamValue spec = makeList(makeSymbol("scripts/i1"));
    ScamValue result = importToEnv(spec);
    ASSERT_TRUE(isEnv(result)) << writeValue(result) << "\n";

    ScamValue xval = asEnv(result)->get(makeSymbol("x"));
    expectInteger(xval, 4, "4", true);
}

TEST_F(LibraryTest, ImportPlainCode)
{
    ScamValue spec = makeList(makeSymbol("scripts/lib1"));
    ScamValue result = importToEnv(spec);
    ASSERT_TRUE(isEnv(result));

    Env * env = asEnv(result);
    ScamValue xval = env->get(makeSymbol("x"));
    expectInteger(xval, 4, "4", true);

    set<string> keys;
    env->getKeys(keys);
    EXPECT_EQ(2, keys.size());
}

TEST_F(LibraryTest, ImportLibraryWithoutExport)
{
    readEvalFile("scripts/lib2.scm");
    ScamValue result = engine.findLibrary(makeList(makeSymbol("lib2")));
    ASSERT_TRUE(isEnv(result)) << writeValue(result);

    Env * env = asEnv(result);
    ScamValue xval = env->get(makeSymbol("x"));
    expectProcedure(xval, "(lambda () (y))");

    ScamValue finalValue = apply(xval, makeNull());
    expectInteger(finalValue, 4, "4", true);

    set<string> keys;
    env->getKeys(keys);
    EXPECT_EQ(2, keys.size());
}

TEST_F(LibraryTest, ImportLibraryWithSimpleExport)
{
    readEvalFile("scripts/lib3.scm");
    ScamValue result = engine.findLibrary(makeList(makeSymbol("lib3")));
    ASSERT_TRUE(isEnv(result)) << writeValue(result);

    Env * env = asEnv(result);
    ScamValue xval = env->get(makeSymbol("x"));
    expectProcedure(xval, "(lambda () (y))");

    ScamValue finalValue = apply(xval, makeNull());
    expectInteger(finalValue, 4, "4", true);

    set<string> keys;
    env->getKeys(keys);
    EXPECT_EQ(1, keys.size());
}

TEST_F(LibraryTest, ImportLibraryWithRenameExport)
{
    readEvalFile("scripts/lib4.scm");
    ScamValue result = engine.findLibrary(makeList(makeSymbol("lib4")));
    ASSERT_TRUE(isEnv(result)) << writeValue(result);

    Env * env = asEnv(result);
    ScamValue xval = env->get(makeSymbol("xray"));
    expectProcedure(xval, "(lambda () (y))");

    ScamValue finalValue = apply(xval, makeNull());
    expectInteger(finalValue, 4, "4", true);

    set<string> keys;
    env->getKeys(keys);
    EXPECT_EQ(1, keys.size());
}

TEST_F(LibraryTest, ImportLoadsOnDemand)
{
    ScamValue result = readEvalFile("scripts/test-demand.scm");
    expectInteger(result, 25, "25", true);
}
