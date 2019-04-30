#include "TestBase.hpp"

#include "expr/ExpressionFactory.hpp"
#include "input/AlternativeParser.hpp"
#include "input/ArgParser.hpp"
#include "input/CountedListParser.hpp"
#include "input/FunctionDefParser.hpp"
#include "input/ListParser.hpp"
#include "input/ParameterListParser.hpp"
#include "input/SequenceParser.hpp"
#include "input/SingletonParser.hpp"
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

TEST_F(ArgParserTest, AcceptOneToThreeIntegers)
{
    IntegerParser     * ip = mm.make<IntegerParser>();
    CountedListParser * parser =
        mm.make<CountedListParser>(ip, 1, 3);

    rejectParse(parser, "()");

    acceptParse(parser, "(1)");
    EXPECT_EQ(1u, parser->size());

    acceptParse(parser, "(1 -1)");
    EXPECT_EQ(2u, parser->size());

    acceptParse(parser, "(1 -1 70055)");
    EXPECT_EQ(3u, parser->size());

    rejectParse(parser, "(-5 -4 -3 -2)");
    EXPECT_EQ(nullptr, parser->get(0));

    rejectParse(parser, "(-3.75 -2)");
}

TEST_F(ArgParserTest, AcceptSingletonAnything)
{
    ArgParser       * ap = mm.make<ArgParser>();
    SingletonParser * parser = mm.make<SingletonParser>(ap);

    rejectParse(parser, "()");

    acceptParse(parser, "(#(just 1 vector))");
    EXPECT_EQ(1u, parser->getValue()->length());

    rejectParse(parser, "(two things)");
}

TEST_F(ArgParserTest, AcceptEmptySequence)
{
    SequenceParser * parser = mm.make<SequenceParser>();

    acceptParse(parser, "()");

    rejectParse(parser, "(item)");
}

TEST_F(ArgParserTest, AcceptSingleKeyword)
{
    const ScamKeyword * kw = ExpressionFactory::makeKeyword(":sample");
    KeywordParser  * kp = mm.make<KeywordParser>(kw);
    SequenceParser * parser = mm.make<SequenceParser>(kp);

    acceptParse(parser, "(:sample)");

    rejectParse(parser, "(:incorrect)");
    rejectParse(parser, "(:sample plus incorrect)");
}

TEST_F(ArgParserTest, AcceptSeveral)
{
    const ScamKeyword * kw = ExpressionFactory::makeKeyword(":sample");
    KeywordParser  * kp = mm.make<KeywordParser>(kw);
    IntegerParser  * ip = mm.make<IntegerParser>();
    SequenceParser * parser =
        mm.make<SequenceParser>(kp, ip);

    acceptParse(parser, "(:sample 23)");

    rejectParse(parser, "(:sample)");
    rejectParse(parser, "(:sample 23 99)");
    rejectParse(parser, "(:sample 23.5)");
}

TEST_F(ArgParserTest, RejectEmptyAlternative)
{
    AlternativeParser * parser =
        mm.make<AlternativeParser>();

    rejectParse(parser, "()");
    rejectParse(parser, "(:sample)");
    rejectParse(parser, "(:sample 23 99)");
}

TEST_F(ArgParserTest, AcceptSingleAlternative)
{
    IntegerParser     * ip = mm.make<IntegerParser>();
    AlternativeParser * parser =
        mm.make<AlternativeParser>(ip);

    acceptParse(parser, "9123913");

    rejectParse(parser, "(-823)");
    rejectParse(parser, "1.234");
}

TEST_F(ArgParserTest, AcceptOneOfManyAlternatives)
{
    const ScamKeyword * kw1 = ExpressionFactory::makeKeyword(":kw1");
    const ScamKeyword * kw2 = ExpressionFactory::makeKeyword(":kw2");

    IntegerParser     * ip = mm.make<IntegerParser>();
    BooleanParser     * bp = mm.make<BooleanParser>();
    KeywordParser     * kp1 = mm.make<KeywordParser>(kw1);
    KeywordParser     * kp2 = mm.make<KeywordParser>(kw2);
    AlternativeParser * parser = mm.make<AlternativeParser>(ip, bp, kp1, kp2);

    acceptParse(parser, "9123913");
    acceptParse(parser, "#FaLSe");
    acceptParse(parser, ":kw1");

    acceptParse(parser, ":kw2");
    EXPECT_EQ(kp2, parser->getMatch());

    rejectParse(parser, "(-823)");
    rejectParse(parser, "1.234");
    rejectParse(parser, ":someOtherKeyword");
}

TEST_F(ArgParserTest, NilParameterList)
{
    ParameterListParser * parser = mm.make<ParameterListParser>();

    acceptParse(parser, "()");
    EXPECT_EQ(0u, parser->size());
}

TEST_F(ArgParserTest, StandardParameterList)
{
    ParameterListParser * parser = mm.make<ParameterListParser>();

    acceptParse(parser, "(a b c)");
    EXPECT_EQ(3u, parser->size());
    expectSymbol(parser->get(2), "c");
}

TEST_F(ArgParserTest, ParameterListWithRest)
{
    ParameterListParser * parser = mm.make<ParameterListParser>();

    acceptParse(parser, "(a b . c)");
    EXPECT_EQ(3u, parser->size());
    expectSymbol(parser->get(0), "a");
    expectSymbol(parser->getRest(), "c");
}

TEST_F(ArgParserTest, ParameterListBareSymbol)
{
    ParameterListParser * parser = mm.make<ParameterListParser>();

    acceptParse(parser, "x");
    EXPECT_EQ(1u, parser->size());
    expectSymbol(parser->get(0), "x");
    expectSymbol(parser->getRest(), "x");
}

TEST_F(ArgParserTest, ParameterListDuplicates)
{
    ParameterListParser * parser = mm.make<ParameterListParser>();

    rejectParse(parser, "(a b c d a)");
}

TEST_F(ArgParserTest, TrivialLambda)
{
    LambdaParser * parser = mm.make<LambdaParser>();

    acceptParse(parser, "(())");
    EXPECT_EQ(0u, parser->getArgs()->size());
    EXPECT_EQ(0u, parser->getFormCount());
}

TEST_F(ArgParserTest, NonTrivialLambda)
{
    LambdaParser * parser = mm.make<LambdaParser>();

    acceptParse(parser, "((arg) (* arg 3))");
    EXPECT_EQ(1u, parser->getArgs()->size());
    EXPECT_EQ(1u, parser->getFormCount());
}

TEST_F(ArgParserTest, TrivialFunctionDef)
{
    FunctionDefParser * parser = mm.make<FunctionDefParser>();

    acceptParse(parser, "(foo ())");
    expectSymbol(parser->getName(), "foo");
    const LambdaParser * lambda = parser->getLambda();
    EXPECT_EQ(0u, lambda->getArgs()->size());
    EXPECT_EQ(0u, lambda->getFormCount());
}

TEST_F(ArgParserTest, NoArgFunctionDef)
{
    FunctionDefParser * parser = mm.make<FunctionDefParser>();

    acceptParse(parser, "(answer () 42)");
    expectSymbol(parser->getName(), "answer");
    const LambdaParser * lambda = parser->getLambda();
    EXPECT_EQ(0u, lambda->getArgs()->size());
    EXPECT_EQ(1u, lambda->getFormCount());
    expectInteger(lambda->getForm(0), 42, "42");
}

TEST_F(ArgParserTest, GeneralFunctionDef)
{
    FunctionDefParser * parser = mm.make<FunctionDefParser>();

    acceptParse(parser, "(stuff (x y . z) (side-effect) (if (> x y) z 1))");
    expectSymbol(parser->getName(), "stuff");

    const LambdaParser * lambda = parser->getLambda();
    const auto args = lambda->getArgs();
    EXPECT_EQ(3u, args->size());
    expectSymbol(args->get(2), "z");
    expectSymbol(args->getRest(), "z");

    EXPECT_EQ(2u, lambda->getFormCount());
}
