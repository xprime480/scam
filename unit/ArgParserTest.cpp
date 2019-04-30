#include "TestBase.hpp"

#include "expr/ExpressionFactory.hpp"
#include "input/ArgParser.hpp"

#include "util/DebugTrace.hpp"

using namespace std;
using namespace scam;

class ArgParserTest : public TestBase
{
protected:
    void acceptParse(ArgParser * parser, const char * text)
    {
        scamTrace("about to read text:", text);

        ExprHandle value = readString(text);
        scamTrace("read the text, got", value, value->toString());

        bool accept = parser->accept(value);
        scamTrace("parser returned", accept);

        ASSERT_TRUE(accept);
        ExprHandle saved = parser->getValue();
        scamTrace("parser saved", saved,
                  (nullptr == saved ? "<null>" : saved->toString()));
        EXPECT_TRUE(value->equals(saved));
    }

    void rejectParse(ArgParser * parser, const char * text)
    {
        ExprHandle value = readString(text);

        bool accept = parser->accept(value);
        EXPECT_FALSE(accept);
        EXPECT_EQ(nullptr, parser->getValue());
    }
};

TEST_F(ArgParserTest, AcceptNonNull)
{
    ArgParser * parser = mm.make<ArgParser>();

    acceptParse(parser, "()");
    acceptParse(parser, "1");
    acceptParse(parser, "1.234");
    acceptParse(parser, "#true");
    acceptParse(parser, "x");
    // acceptParse(parser, "\"a string here\"");
    acceptParse(parser, "#\\x");
    acceptParse(parser, "#(a b c)");
    acceptParse(parser, "(list should work)");

    rejectParse(parser, "");
}
