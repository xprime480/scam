#include "TestBase.hpp"

#include "env/Env.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/Import.hpp"
#include "port/FilePort.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;

class ImportTest : public TestBase
{
protected:
    ImportTest()
    {
    }

    void SetUp() override
    {
        TestBase::SetUp();

        // FilePort * port = new FilePort("analysis.txt", ScamPort::Writeable);
        // configEnv->put(makeSymbol("**log-port**"), makePort(port));
    }
};

TEST_F(ImportTest, NestedImport)
{
    ScamValue spec = makeList(makeSymbol("scripts/i1"));
    ScamValue result = importToEnv(spec, &engine);
    ASSERT_TRUE(isEnv(result));

    ScamValue xval = asEnv(result)->get(makeSymbol("x"));
    expectInteger(xval, 4, "4", true);
}

TEST_F(ImportTest, ImportLibrary)
{
    ScamValue spec = makeList(makeSymbol("scripts/lib1"));
    ScamValue result = importToEnv(spec, &engine);
    ASSERT_TRUE(isEnv(result));

    Env * env = asEnv(result);
    ScamValue xval = env->get(makeSymbol("x"));
    expectInteger(xval, 4, "4", true);

    set<string> keys;
    env->getKeys(keys);
    EXPECT_EQ(2, keys.size());
}

TEST_F(ImportTest, ImportLibraryWithExport)
{
    ScamValue spec = makeList(makeSymbol("scripts/lib2"));
    ScamValue result = importToEnv(spec, &engine);
    ASSERT_TRUE(isEnv(result));

    Env * env = asEnv(result);
    ScamValue xval = env->get(makeSymbol("x"));
    expectInteger(xval, 4, "4", true);

    set<string> keys;
    env->getKeys(keys);
    EXPECT_EQ(1, keys.size());
}
