#include "TestBase.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamToInternal.hpp"
#include "input/AlternativeParser.hpp"
#include "input/ApplyParser.hpp"
#include "input/ArgParser.hpp"
#include "input/BindFormParser.hpp"
#include "input/ClassDefParser.hpp"
#include "input/CountedListParser.hpp"
#include "input/DictOpsParser.hpp"
#include "input/ExtendedNumericParser.hpp"
#include "input/FunctionDefParser.hpp"
#include "input/IncludeParser.hpp"
#include "input/LetParser.hpp"
#include "input/ListParser.hpp"
#include "input/MatchUnifyParser.hpp"
#include "input/NumericListParser.hpp"
#include "input/ParameterListParser.hpp"
#include "input/RelopsListParser.hpp"
#include "input/SequenceParser.hpp"
#include "input/SingletonParser.hpp"
#include "input/SubstituteParser.hpp"
#include "input/SymbolPlusManyParser.hpp"
#include "input/SymbolPlusParser.hpp"
#include "input/TypeParsers.hpp"
#include "input/VrefParser.hpp"

using namespace std;
using namespace scam;

class ArgParserTest : public TestBase
{
protected:
    void acceptParse(ArgParser * parser, const char * text)
    {
        ScamValue value = readString(text);
        bool accept = parser->accept(value);

        ASSERT_TRUE(accept);
        ScamValue saved = parser->getValue();
        EXPECT_TRUE(value->equals(saved));
    }

    void rejectParse(ArgParser * parser, const char * text)
    {
        ScamValue value = readString(text);

        bool accept = parser->accept(value);
        EXPECT_FALSE(accept);
        ConstScamValue v = parser->getValue();
        expectNull(v);
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

    ListParser * parser = getListOfAnythingParser();

    acceptParse(parser, "()");
    EXPECT_EQ(0u, parser->size());

    acceptParse(parser, "(a symbol (+ 2 2) #\\c)");
    EXPECT_EQ(4u, parser->size());
    expectSymbol(parser->get(1), text);
    expectNull(parser->get(4));

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

TEST_F(ArgParserTest, AcceptOneToThreeNumbers)
{
    NumericParser     * ip = mm.make<NumericParser>();
    CountedListParser * parser =
        mm.make<CountedListParser>(ip, 1, 3);

    rejectParse(parser, "()");

    acceptParse(parser, "(1)");
    EXPECT_EQ(1u, parser->size());

    acceptParse(parser, "(1 -1/2)");
    EXPECT_EQ(2u, parser->size());

    acceptParse(parser, "(1 -1 700.55)");
    EXPECT_EQ(3u, parser->size());

    rejectParse(parser, "(-5 -4 -3 -2)");
    expectNull(parser->get(0));
}

TEST_F(ArgParserTest, AcceptSingletonAnything)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
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
    KeywordParser  * kp     = mm.make<KeywordParser>(kw);
    NumericParser  * ip     = mm.make<NumericParser>();
    SequenceParser * parser = mm.make<SequenceParser>(kp, ip);

    acceptParse(parser, "(:sample 23)");

    rejectParse(parser, "(:sample)");
    rejectParse(parser, "(:sample 23 99)");
}

TEST_F(ArgParserTest, RejectEmptyAlternative)
{
    AlternativeParser * parser = mm.make<AlternativeParser>();

    rejectParse(parser, "()");
    rejectParse(parser, "(:sample)");
    rejectParse(parser, "(:sample 23 99)");
}

TEST_F(ArgParserTest, AcceptSingleAlternative)
{
    NumericParser     * ip     = mm.make<NumericParser>();
    AlternativeParser * parser = mm.make<AlternativeParser>(ip);

    acceptParse(parser, "9123913");
    rejectParse(parser, "(-823)");
}

TEST_F(ArgParserTest, AcceptOneOfManyAlternatives)
{
    const ScamKeyword * kw1 = ExpressionFactory::makeKeyword(":kw1");
    const ScamKeyword * kw2 = ExpressionFactory::makeKeyword(":kw2");

    NumericParser     * ip     = mm.make<NumericParser>();
    BooleanParser     * bp     = mm.make<BooleanParser>();
    KeywordParser     * kp1    = mm.make<KeywordParser>(kw1);
    KeywordParser     * kp2    = mm.make<KeywordParser>(kw2);
    AlternativeParser * parser = mm.make<AlternativeParser>(ip, bp, kp1, kp2);

    acceptParse(parser, "9123913");
    acceptParse(parser, "#FaLSe");
    acceptParse(parser, ":kw1");

    acceptParse(parser, ":kw2");
    EXPECT_EQ(kp2, parser->getMatch());

    rejectParse(parser, "(-823)");
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
    expectInteger(lambda->getForm(0), 42, "42", true);
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

TEST_F(ArgParserTest, ApplyTooFew)
{
    ApplyParser * parser = mm.make<ApplyParser>();

    rejectParse(parser, "(+)");
}

TEST_F(ArgParserTest, ApplyTooMany)
{
    ApplyParser * parser = mm.make<ApplyParser>();

    rejectParse(parser, "(+ 2 3)");
}

TEST_F(ArgParserTest, ApplyCorrect)
{
    ApplyParser * parser = mm.make<ApplyParser>();

    acceptParse(parser, "(+ (2 3))");
    expectSymbol(parser->getParsedOp(), "+");
    expectList(parser->getArgs(), "(2 3)", 2);
}

TEST_F(ArgParserTest, TrivialClass)
{
    ClassDefParser * parser = mm.make<ClassDefParser>();

    acceptParse(parser, "(Root ())");

    expectSymbol(parser->getBase(), "Root");
    EXPECT_EQ(0u, parser->getVarCount());
    EXPECT_EQ(0u, parser->getMethodCount());
}

TEST_F(ArgParserTest, SmallClass)
{
    ClassDefParser * parser = mm.make<ClassDefParser>();

    acceptParse(parser, "\
(Jerry \
  (a b) \
  (get-a () a) \
  (set-b (new-b) \
      (assign! b new-b)))\
");

    expectSymbol(parser->getBase(), "Jerry");

    EXPECT_EQ(2u, parser->getVarCount());
    expectSymbol(parser->getVar(1), "b");

    EXPECT_EQ(2u, parser->getMethodCount());
    expectSymbol(parser->getMethod(0u)->getName(), "get-a");
}

TEST_F(ArgParserTest, BadClassDef)
{
    ClassDefParser * parser = mm.make<ClassDefParser>();

    rejectParse(parser, "(Base ok not-a-function)");
}

TEST_F(ArgParserTest, DictGet)
{
    DictOpsParser * parser = mm.make<DictOpsParser>();

    acceptParse(parser, "(:get anything)");
    expectKeyword(parser->getParsedOp(), ":get");
    expectSymbol(parser->getOpKey(), "anything");
    expectNull(parser->getOpVal());
}

TEST_F(ArgParserTest, DictPut)
{
    DictOpsParser * parser = mm.make<DictOpsParser>();

    acceptParse(parser, "(:put anything (compute new value))");
    expectKeyword(parser->getParsedOp(), ":put");
    expectSymbol(parser->getOpKey(), "anything");
    expectList(parser->getOpVal(), "(compute new value)", 3);
}

TEST_F(ArgParserTest, DictLength)
{
    DictOpsParser * parser = mm.make<DictOpsParser>();

    acceptParse(parser, "(:length)");
    expectKeyword(parser->getParsedOp(), ":length");
    expectNull(parser->getOpKey());
    expectNull(parser->getOpVal());
}

TEST_F(ArgParserTest, DictHas)
{
    DictOpsParser * parser = mm.make<DictOpsParser>();

    acceptParse(parser, "(:has anything)");
    expectKeyword(parser->getParsedOp(), ":has");
    expectSymbol(parser->getOpKey(), "anything");
    expectNull(parser->getOpVal());
}

TEST_F(ArgParserTest, DictRemove)
{
    DictOpsParser * parser = mm.make<DictOpsParser>();

    acceptParse(parser, "(:remove anything)");
    expectKeyword(parser->getParsedOp(), ":remove");
    expectSymbol(parser->getOpKey(), "anything");
    expectNull(parser->getOpVal());
}

TEST_F(ArgParserTest, DictTooMany)
{
    DictOpsParser * parser = mm.make<DictOpsParser>();
    rejectParse(parser, "(:get has too many arguments)");
}

TEST_F(ArgParserTest, DictTooFew)
{
    DictOpsParser * parser = mm.make<DictOpsParser>();
    rejectParse(parser, "(:put too-few-arguments)");
}

TEST_F(ArgParserTest, DictUnknown)
{
    DictOpsParser * parser = mm.make<DictOpsParser>();
    rejectParse(parser, "(:unknown)");
}

TEST_F(ArgParserTest, SymbolPlus)
{
    SymbolPlusParser * parser = mm.make<SymbolPlusParser>();
    acceptParse(parser, "(answer (* 2 21))");
    expectSymbol(parser->getSymbol(), "answer");
}

TEST_F(ArgParserTest, SymbolWithoutForm)
{
    SymbolPlusParser * parser = mm.make<SymbolPlusParser>();
    rejectParse(parser, "(answer)");
}

TEST_F(ArgParserTest, SymbolPlusMany)
{
    SymbolPlusManyParser * parser = mm.make<SymbolPlusManyParser>();
    acceptParse(parser, "(answer (* 2 21))");
    expectSymbol(parser->getSymbol(), "answer");
}

TEST_F(ArgParserTest, SymbolManyWithoutForm)
{
    SymbolPlusManyParser * parser = mm.make<SymbolPlusManyParser>();
    acceptParse(parser, "(answer)");
    expectSymbol(parser->getSymbol(), "answer");
    expectNil(parser->getForms());
}

TEST_F(ArgParserTest, BindFormSymbolOnly)
{
    BindFormParser * parser = mm.make<BindFormParser>();
    rejectParse(parser, "(answer)");
}

TEST_F(ArgParserTest, BindFormSymbolPlusForm)
{
    BindFormParser * parser = mm.make<BindFormParser>();
    acceptParse(parser, "(answer 42)");
    expectSymbol(parser->getSymbol(), "answer");
    expectInteger(parser->getForm(), 42, "42", true);
}

TEST_F(ArgParserTest, TrivialLet)
{
    LetParser * parser = mm.make<LetParser>();
    acceptParse(parser, "(())");
    EXPECT_EQ(0u, parser->getBindingCount());
    expectNil(parser->getForms());
}

TEST_F(ArgParserTest, NonTrivialLet)
{
    LetParser * parser = mm.make<LetParser>();
    acceptParse(parser, "(((a 3) (b a)) (+ a 2))");

    EXPECT_EQ(2u, parser->getBindingCount());

    BindFormParser * binding = parser->getBinding(0u);
    expectSymbol(binding->getSymbol(), "a");
    expectInteger(binding->getForm(), 3, "3", true);

    binding = parser->getBinding(1u);
    expectSymbol(binding->getSymbol(), "b");
    expectSymbol(binding->getForm(), "a");

    expectList(parser->getForms(), "((+ a 2))", 1);
}

TEST_F(ArgParserTest, LetWithBadBindings)
{
    LetParser * parser = mm.make<LetParser>();
    rejectParse(parser, "(this-is-not-a-binding 2 2 2 3 3 4)");
}

TEST_F(ArgParserTest, IncludeParserEmpty)
{
    IncludeParser * parser = mm.make<IncludeParser>();
    rejectParse(parser, "()");
}

TEST_F(ArgParserTest, IncludeParserOne)
{
    IncludeParser * parser = mm.make<IncludeParser>();
    acceptParse(parser, "(\"test\")");
    expectString(parser->get(0), "test");
}

TEST_F(ArgParserTest, IncludeParserSeveral)
{
    IncludeParser * parser = mm.make<IncludeParser>();
    acceptParse(parser, "(\"test\" \"best\" \"rest\")");
    expectString(parser->get(2), "rest");
}

TEST_F(ArgParserTest, IncludeParserNonString)
{
    IncludeParser * parser = mm.make<IncludeParser>();
    rejectParse(parser, "(\"test\" \"best\" \"rest\" 999)");
}

TEST_F(ArgParserTest, MatchUnifyParserMatchValid)
{
    MatchUnifyParser * parser = mm.make<MatchUnifyParser>(true);
    acceptParse(parser, "(thing-1 thing-2)");
    EXPECT_TRUE(parser->isMatch());
    expectSymbol(parser->getLhs(), "thing-1");
    expectSymbol(parser->getRhs(), "thing-2");
    expectDict(parser->getDict(), 0, "{}");
}

TEST_F(ArgParserTest, MatchUnifyParserMatchInvalid )
{
    MatchUnifyParser * parser = mm.make<MatchUnifyParser>(true);
    rejectParse(parser, "(thing-1 thing-2 {})");
    EXPECT_TRUE(parser->isMatch());
}

TEST_F(ArgParserTest, MatchUnifyParserUnifyValidWithoutDict)
{
    MatchUnifyParser * parser = mm.make<MatchUnifyParser>(false);
    acceptParse(parser, "(thing-1 thing-2)");
    EXPECT_FALSE(parser->isMatch());
    expectSymbol(parser->getLhs(), "thing-1");
    expectSymbol(parser->getRhs(), "thing-2");
    expectDict(parser->getDict(), 0, "{}");
}

TEST_F(ArgParserTest, MatchUnifyParserUnifyValidWitDict)
{
    MatchUnifyParser * parser = mm.make<MatchUnifyParser>(false);
    acceptParse(parser, "(thing-1 thing-2 { :key 3 })");
    EXPECT_FALSE(parser->isMatch());
    expectSymbol(parser->getLhs(), "thing-1");
    expectSymbol(parser->getRhs(), "thing-2");
    expectDict(parser->getDict(), 1, "{ :key 3 }");
}

TEST_F(ArgParserTest, NumericEmptyList)
{
    NumericListParser * parser = mm.make<NumericListParser>();
    acceptParse(parser, "()");
    EXPECT_EQ(0, parser->size());
}

TEST_F(ArgParserTest, NumericMixedList)
{
    NumericListParser * parser = mm.make<NumericListParser>();
    acceptParse(parser, "(1.2 3 -750.0 0)");
    EXPECT_EQ(4, parser->size());
    RationalPair value { 6, 5 };
    expectRational(parser->get(0), value, "6/5", false);
    expectInteger(parser->get(1), 3, "3", true);
    expectInteger(parser->get(2), -750, "-750", false);
    expectInteger(parser->get(3), 0, "0", true);
}

TEST_F(ArgParserTest, ExtendedNumericList)
{
    NumericListParser * parser = mm.make<NumericListParser>();
    acceptParse(parser, "(-inf.0 +inf.0 +nan.0)");
    EXPECT_EQ(3, parser->size());
}

TEST_F(ArgParserTest, NumericInvalidLIst)
{
    NumericListParser * parser = mm.make<NumericListParser>();
    rejectParse(parser, "(1.2 3 -750 spoiler 0)");
}

TEST_F(ArgParserTest, RelopsEmptyList)
{
    RelopsListParser * parser = mm.make<RelopsListParser>();
    acceptParse(parser, "()");
    EXPECT_EQ(0, parser->size());
}

TEST_F(ArgParserTest, RelopsListOfNumbers)
{
    RelopsListParser * parser = mm.make<RelopsListParser>();
    acceptParse(parser, "(1 2 3 4 5)");
    EXPECT_EQ(5, parser->size());
    expectInteger(parser->get(2), 3, "3", true);
}

TEST_F(ArgParserTest, RelopsListOfStrings)
{
    RelopsListParser * parser = mm.make<RelopsListParser>();
    acceptParse(parser, "(\"hello\" \"world\")");
    EXPECT_EQ(2, parser->size());
    expectString(parser->get(0), "hello");
}

TEST_F(ArgParserTest, RelopsListMixed)
{
    RelopsListParser * parser = mm.make<RelopsListParser>();
    rejectParse(parser, "(3.14159 \"hello\" \"world\")");
}

TEST_F(ArgParserTest, RelopsListOfExtendedNumbers)
{
    RelopsListParser * parser = mm.make<RelopsListParser>();
    acceptParse(parser, "(3.14159 -1 +inf.0 -inf.0 +nan.0)");
    EXPECT_EQ(5, parser->size());
}

TEST_F(ArgParserTest, SubstituteParserOK)
{
    SubstituteParser * parser = mm.make<SubstituteParser>();
    acceptParse(parser, "(:cat { :cat 23 })");
    expectKeyword(parser->getForm(), ":cat");
    expectDict(parser->getDict(), 1, "{ :cat 23 }");
}

TEST_F(ArgParserTest, VrefParserOK)
{
    VrefParser * parser = mm.make<VrefParser>();
    acceptParse(parser, "(17 #(1 2 3 :cat :dog))");
    EXPECT_EQ(17, parser->getIndex());
    expectVector(parser->getVector(), "#(1 2 3 :cat :dog)", 5);
}

TEST_F(ArgParserTest, VrefNegativeIndex)
{
    VrefParser * parser = mm.make<VrefParser>();
    rejectParse(parser, "(-17 #(1 2 3 :cat :dog))");
}

TEST_F(ArgParserTest, ExtendedNumericInteger)
{
    ExtendedNumericParser * parser = mm.make<ExtendedNumericParser>();
    acceptParse(parser, "23");
    expectInteger(parser->getValue(), 23, "23", true);
}

TEST_F(ArgParserTest, ExtendedNumericNegInf)
{
    ExtendedNumericParser * parser = mm.make<ExtendedNumericParser>();
    acceptParse(parser, "-inf.0");
    expectSpecialNumeric(parser->getValue(), "-inf.0");
}

TEST_F(ArgParserTest, ExtendedNumericPosInf)
{
    ExtendedNumericParser * parser = mm.make<ExtendedNumericParser>();
    acceptParse(parser, "+inf.0");
    expectSpecialNumeric(parser->getValue(), "+inf.0");
}

TEST_F(ArgParserTest, ExtendedNumericNaN)
{
    ExtendedNumericParser * parser = mm.make<ExtendedNumericParser>();
    acceptParse(parser, "+nan.0");
    expectSpecialNumeric(parser->getValue(), "+nan.0");
}

TEST_F(ArgParserTest, ExtendedNumericNotNumeric)
{
    ExtendedNumericParser * parser = mm.make<ExtendedNumericParser>();
    rejectParse(parser, "just-a-symbol");
}
