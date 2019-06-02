#include "TestBase.hpp"

#include "Extractor.hpp"
#include "expr/ValueFactory.hpp"
#include "prim/StringOps.hpp"
#include "util/MemoryManager.hpp"

using namespace std;
using namespace scam;

class StringOpsTest : public TestBase
{
protected:
    StringOpsTest()
        : context("StringOpsTest")
        , cont(standardMemoryManager.make<Extractor>())
    {
    }

    const string context;
    Extractor * cont;
};

TEST_F(StringOpsTest, UpcaseEmptyString)
{
    const string text { "\"\"" };
    ScamValue arg1 = readString(text.c_str());
    ScamValue args = makeList(arg1);

    applyStringUpcase(args, cont, &engine);
    expectString(cont->getExpr(), text);
}

TEST_F(StringOpsTest, UpcaseNonEmptyString)
{
    const string text { "\"aBcD\"" };
    const string ucText { "\"ABCD\"" };
    ScamValue arg1 = readString(text.c_str());
    ScamValue args = makeList(arg1);

    applyStringUpcase(args, cont, &engine);
    expectString(cont->getExpr(), ucText);
}

TEST_F(StringOpsTest, DowncaseEmptyString)
{
    const string text { "\"\"" };
    ScamValue arg1 = readString(text.c_str());
    ScamValue args = makeList(arg1);

    applyStringDowncase(args, cont, &engine);
    expectString(cont->getExpr(), text);
}

TEST_F(StringOpsTest, DowncaseNonEmptyString)
{
    const string text { "\"aBcD\"" };
    const string ucText { "\"abcd\"" };
    ScamValue arg1 = readString(text.c_str());
    ScamValue args = makeList(arg1);

    applyStringDowncase(args, cont, &engine);
    expectString(cont->getExpr(), ucText);
}
