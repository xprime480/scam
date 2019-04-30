#include "TestBase.hpp"

#include "expr/ExpressionFactory.hpp"
#include "input/ArgParser.hpp"
#include "input/ListParser.hpp"
#include "input/TypeParsers.hpp"

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

TEST_F(ArgParserTest, AcceptNil)
{
    NilParser * parser = mm.make<NilParser>();
    acceptParse(parser, "()");
}

TEST_F(ArgParserTest, AcceptAnyInteger)
{
    IntegerParser * parser = mm.make<IntegerParser>();

    acceptParse(parser, "-234134");
    acceptParse(parser, "0");
    acceptParse(parser, "1");
    acceptParse(parser, "42");
}

TEST_F(ArgParserTest, AcceptOneInteger)
{
    const ScamInteger * target = ExpressionFactory::makeInteger(42);
    IntegerParser * parser = mm.make<IntegerParser>(target);

    rejectParse(parser, "-234134");
    rejectParse(parser, "0");
    rejectParse(parser, "1");
    acceptParse(parser, "42");
}

TEST_F(ArgParserTest, RejectOneInteger)
{
    const ScamInteger * target = ExpressionFactory::makeInteger(42);
    IntegerParser * parser =
        mm.make<IntegerParser>(target, true);

    acceptParse(parser, "-234134");
    acceptParse(parser, "0");
    acceptParse(parser, "1");
    rejectParse(parser, "42");
}

TEST_F(ArgParserTest, AcceptAnyNumeric)
{
    NumericParser * parser = mm.make<NumericParser>();

    acceptParse(parser, "-234134");
    acceptParse(parser, "0.00001");
    rejectParse(parser, "symbol");
}

TEST_F(ArgParserTest, AcceptListOfAnything)
{
    const char * text = "symbol";

    ArgParser *  ap = mm.make<ArgParser>();
    ListParser * parser = mm.make<ListParser>(ap);

    acceptParse(parser, "()");
    EXPECT_EQ(0u, parser->size());

    acceptParse(parser, "(a symbol (+ 2 2) #\\c)");
    EXPECT_EQ(4u, parser->size());
    expectSymbol(parser->get(1), text);
    EXPECT_EQ(nullptr, parser->get(4));

    rejectParse(parser, text);
}

TEST_F(ArgParserTest, AcceptListOfBooleans)
{
    BooleanParser * bp = mm.make<BooleanParser>();
    ListParser    * parser = mm.make<ListParser>(bp);

    acceptParse(parser, "()");
    EXPECT_EQ(0u, parser->size());

    acceptParse(parser, "(#t #FALSE #t #f)");
    EXPECT_EQ(4u, parser->size());

    rejectParse(parser, "(#t #FALSE i-am-not-a-boolean #f)");
}
