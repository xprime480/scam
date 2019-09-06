#include "TestBase.hpp"

#include "util/ClassDef.hpp"
#include "util/DictCommand.hpp"
#include "util/FunctionDef.hpp"
#include "util/InstanceDef.hpp"
#include "util/LambdaDef.hpp"
#include "util/LetDef.hpp"
#include "util/Parameter.hpp"

using namespace std;
using namespace scam;

class ParameterTest : public TestBase
{
protected:
    ParameterTest()
    {
    }
};

TEST_F(ParameterTest, ObjectParameterWithNullInput)
{
    ObjectParameter parm;
    ScamValue args = readString("()");
    ScamValue result = parm.transform(args);
    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, ObjectParameterWithStringInput)
{
    ObjectParameter parm;
    ScamValue args = readString("(\"hello\")");
    ScamValue result = parm.transform(args);
    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectString(parm.value, "\"hello\"");
}

TEST_F(ParameterTest, ObjectParameterWithIntegerInput)
{
    ObjectParameter parm;
    ScamValue args = readString("(1)");
    ScamValue result = parm.transform(args);
    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectInteger(parm.value, 1, "1", true);
}

TEST_F(ParameterTest, ObjectParameterWithExtraInput)
{
    ObjectParameter parm;
    ScamValue args = readString("(1 2)");
    ScamValue result = parm.transform(args);
    expectList(result, "(2)", 1);
    EXPECT_TRUE(parm.valid);
    expectInteger(parm.value, 1, "1", true);
}

TEST_F(ParameterTest, IntegerParameterForNonInteger)
{
    IntegerParameter parm;
    ScamValue args = readString("(:no)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, IntegerParameterForInteger)
{
    IntegerParameter parm;
    ScamValue args = readString("(33)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectInteger(parm.value, 33, "33", true);
}

TEST_F(ParameterTest, CountNotInteger)
{
    CountParameter parm;
    ScamValue args = readString("(:hi)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, CountNotExact)
{
    CountParameter parm;
    ScamValue args = readString("(#i3)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, CountNegative)
{
    CountParameter parm;
    ScamValue args = readString("(-5)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, CountZero)
{
    CountParameter parm;
    ScamValue args = readString("(0)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectInteger(parm.value, 0, "0", true);
}

TEST_F(ParameterTest, CountPositive)
{
    CountParameter parm;
    ScamValue args = readString("(12345)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectInteger(parm.value, 12345, "12345", true);
}

TEST_F(ParameterTest, IndexReferantNotValid)
{
    ObjectParameter ref;
    IndexParameter parm(ref);
    ScamValue args = readString("(0)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, IndexTooLow)
{
    ObjectParameter ref;
    ref.valid = true;
    ref.value = makeString("abc");
    IndexParameter parm(ref);
    ScamValue args = readString("(-1)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, IndexTooHigh)
{
    ObjectParameter ref;
    ref.valid = true;
    ref.value = makeString("abc");
    IndexParameter parm(ref);
    ScamValue args = readString("(3)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, IndexOK)
{
    ObjectParameter ref;
    ref.valid = true;
    ref.value = makeString("abc");
    IndexParameter parm(ref);
    ScamValue args = readString("(2)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectInteger(parm.value, 2, "2", true);
}

TEST_F(ParameterTest, StartIndexOK)
{
    ObjectParameter ref;
    ref.valid = true;
    ref.value = makeString("abc");
    StartIndexParameter parm(ref);
    ScamValue args = readString("(3)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectInteger(parm.value, 3, "3", true);
}

TEST_F(ParameterTest, EndIndexOutOfOrder)
{
    ObjectParameter ref;
    ref.valid = true;
    ref.value = makeString("abc");
    StartIndexParameter start(ref);
    start.valid = true;
    start.value = makeInteger(2, true);
    EndIndexParameter parm(ref, start);
    ScamValue args = readString("(1)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, EndIndexOK)
{
    ObjectParameter ref;
    ref.valid = true;
    ref.value = makeString("abc");
    StartIndexParameter start(ref);
    start.valid = true;
    start.value = makeInteger(0, true);
    EndIndexParameter parm(ref, start);
    ScamValue args = readString("(2)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectInteger(parm.value, 2, "2", true);
}

TEST_F(ParameterTest, MutableActuallyImmutable)
{
    StringParameter item;
    MutableParameter parm(item);
    ScamValue args = readString("\"permanent\"");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, MutableOK)
{
    StringParameter item;
    MutableParameter parm(item);
    ScamValue args = readEval("(list (string-copy \"yo!\"))");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectString(parm.value, "\"yo!\"");
}

TEST_F(ParameterTest, OptionalNotSeen)
{
    StringParameter item;
    OptionalParameter parm(item);
    ScamValue args = readString("(1 2)");

    ScamValue result = parm.transform(args);

    expectList(result, "(1 2)", 2);
    EXPECT_TRUE(parm.valid);
    EXPECT_FALSE(parm.found);
    expectNothing(parm.value);
}

TEST_F(ParameterTest, OptionalSeen)
{
    IntegerParameter item;
    OptionalParameter parm(item);
    ScamValue args = readString("(1 2)");

    ScamValue result = parm.transform(args);

    expectList(result, "(2)", 1);
    EXPECT_TRUE(parm.valid);
    EXPECT_TRUE(parm.found);
    expectInteger(parm.value, 1, "1", true);
}

TEST_F(ParameterTest, CountedParameterTooFew)
{
    ObjectParameter item;
    CountedParameter parm(item, 3, 5);
    ScamValue args = readString("(1 2)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, CountedParameterExtra)
{
    ObjectParameter item;
    CountedParameter parm(item, 3, 5);
    ScamValue args = readString("(1 2 3 4 5 6)");

    ScamValue result = parm.transform(args);
    expectList(result, "(6)", 1);
    EXPECT_TRUE(parm.valid);

    expectList(parm.value, "(1 2 3 4 5)", 5);
}

TEST_F(ParameterTest, CountedParameterAtLowerBound)
{
    ObjectParameter item;
    CountedParameter parm(item, 6);
    ScamValue args = readString("(1 2 3 4 5 6)");

    ScamValue result = parm.transform(args);
    expectNull(result);
    EXPECT_TRUE(parm.valid);

    expectList(parm.value, "(1 2 3 4 5 6)", 6);
}

TEST_F(ParameterTest, CountedParameterAtUpperBound)
{
    ObjectParameter item;
    CountedParameter parm(item, 0, 6);
    ScamValue args = readString("(1 2 :anything 4 5 6)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);

    expectList(parm.value, "(1 2 :anything 4 5 6)", 6);
}

TEST_F(ParameterTest, CountedParameterWrongType)
{
    IntegerParameter item;
    CountedParameter parm(item, 3, 6);
    ScamValue args = readString("(1 2 :anything 4 5 6)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, ListOfNotList)
{
    IntegerParameter pInt;
    ListOfParameter parm(pInt);

    ScamValue args = readString("(:kw)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, ListOfIntWithNonInt)
{
    IntegerParameter pInt;
    ListOfParameter parm(pInt);
    ScamValue args = readString("((1 2 3 :kw 4))");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, ListOfIntOK)
{
    IntegerParameter pInt;
    ListOfParameter parm(pInt);
    ScamValue args = readString("((1 2 3 4))");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectList(parm.value, "(1 2 3 4)", 4);
}

TEST_F(ParameterTest, AlternativeNotFound)
{
    IntegerParameter pInt;
    StringParameter  pStr;
    AlternativeParameter parm(pInt, pStr);
    ScamValue args = readString("(:kw)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, AlternativeFirstMatch)
{
    IntegerParameter pInt;
    StringParameter  pStr;
    AlternativeParameter parm(pInt, pStr);
    ScamValue args = readString("(1)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectInteger(parm.value, 1, "1", true);
}

TEST_F(ParameterTest, AlternativeSecondMatch)
{
    IntegerParameter pInt;
    StringParameter  pStr;
    AlternativeParameter parm(pInt, pStr);
    ScamValue args = readString("(\"x\")");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectString(parm.value, "\"x\"");
}

TEST_F(ParameterTest, LambdaDefError)
{
    LambdaDef parm;
    ScamValue args = readString("(:nope)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, LambdaDefTrivial)
{
    LambdaDef parm;
    ScamValue args = readString("(())");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectNull(parm.formals);
    expectNothing(parm.rest);
    expectNull(parm.forms);
}

TEST_F(ParameterTest, LambdaDefFull)
{
    LambdaDef parm;
    ScamValue args = readString("((x . y) (+ x (apply * y)))");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectList(parm.formals, "(x)", 1);
    expectSymbol(parm.rest, "y");
    expectList(parm.forms, "((+ x (apply * y)))", 1);
}

TEST_F(ParameterTest, FunctionDefError)
{
    FunctionDef parm;
    ScamValue args = readString("(too-short)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, FunctionDefTrivial)
{
    FunctionDef parm;
    ScamValue args = readString("(fn ())");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectSymbol(parm.fname, "fn");
    expectNull(parm.lambda.formals);
    expectNothing(parm.lambda.rest);
    expectNull(parm.lambda.forms);
}

TEST_F(ParameterTest, FunctionDefFull)
{
    FunctionDef parm;
    ScamValue args = readString("(fn (x . y) (+ x (apply * y)))");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectSymbol(parm.fname, "fn");
    expectList(parm.lambda.formals, "(x)", 1);
    expectSymbol(parm.lambda.rest, "y");
    expectList(parm.lambda.forms, "((+ x (apply * y)))", 1);
}

TEST_F(ParameterTest, ClassDefError)
{
    ClassDef parm;
    ScamValue args = readString("(Root not-args)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, ClassDefTrivial)
{
    ClassDef parm;
    ScamValue args = readString("(Root ())");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectSymbol(parm.base, "Root");
    expectNull(parm.vars);
    EXPECT_EQ(0, parm.methods.size());
}

TEST_F(ParameterTest, ClassDefFull)
{
    ClassDef parm;
    ScamValue args = readString("\
(Jerry \
  (a b) \
  (get-a () a) \
  (set-b (new-b) \
      (set! b new-b)))\
");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectSymbol(parm.base, "Jerry");
    expectList(parm.vars, "(a b)", 2);
    EXPECT_EQ(2, parm.methods.size());
}

TEST_F(ParameterTest, InstanceError)
{
    InstanceDef parm;
    ScamValue args = readString("()");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, InstanceTrivial)
{
    InstanceDef parm;
    ScamValue args = readString("(foo)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectSymbol(parm.name, "foo");
    expectNull(parm.forms);
}

TEST_F(ParameterTest, InstanceFull)
{
    InstanceDef parm;
    ScamValue args = readString("(foo 1 2 3)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectSymbol(parm.name, "foo");
    expectList(parm.forms, "(1 2 3)", 3);
}

TEST_F(ParameterTest, DictUnknown)
{
    DictCommand parm;
    ScamValue args = readString("(:unknown)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, DictTooFew)
{
    DictCommand parm;
    ScamValue args = readString("(:put too-few-arguments)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, DictTooMany)
{
    DictCommand parm;
    ScamValue args = readString("(:get has too many arguments)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, DictGet)
{
    DictCommand parm;
    ScamValue args = readString("(:get anything)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectKeyword(parm.op, ":get");
    expectSymbol(parm.key, "anything");
    expectNothing(parm.val);
}

TEST_F(ParameterTest, DictPut)
{
    DictCommand parm;
    ScamValue args = readString("(:put anything (compute new value))");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectKeyword(parm.op, ":put");
    expectSymbol(parm.key, "anything");
    expectList(parm.val, "(compute new value)", 3);
}

TEST_F(ParameterTest, DictLength)
{
    DictCommand parm;
    ScamValue args = readString("(:length)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectKeyword(parm.op, ":length");
    expectNothing(parm.key);
    expectNothing(parm.val);
}

TEST_F(ParameterTest, DictHas)
{
    DictCommand parm;
    ScamValue args = readString("(:has anything)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectKeyword(parm.op, ":has");
    expectSymbol(parm.key, "anything");
    expectNothing(parm.val);
}

TEST_F(ParameterTest, DictRemove)
{
    DictCommand parm;
    ScamValue args = readString("(:remove anything)");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectKeyword(parm.op, ":remove");
    expectSymbol(parm.key, "anything");
    expectNothing(parm.val);
}

TEST_F(ParameterTest, LetWithBadBindings)
{
    LetDef parm;
    ScamValue args = readString("(this-is-not-a-binding 2 2 2 3 3 4)");

    ScamValue result = parm.transform(args);

    expectError(result);
    EXPECT_FALSE(parm.valid);
}

TEST_F(ParameterTest, LetTrivial)
{
    LetDef parm;
    ScamValue args = readString("(())");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectNull(parm.formals);
    expectNull(parm.values);
    expectNull(parm.forms);
}

TEST_F(ParameterTest, LetNonTrivial)
{
    LetDef parm;
    ScamValue args = readString("(((a 3) (b (+ a 1))) (+ a 2))");

    ScamValue result = parm.transform(args);

    expectNull(result);
    EXPECT_TRUE(parm.valid);
    expectList(parm.formals, "(a b)", 2);
    expectList(parm.values, "(3 (+ a 1))", 2);
    expectList(parm.forms, "((+ a 2))", 1);
}

/* //////////////////// ********************** //////////////////// */


TEST_F(ParameterTest, ArgToParmsEndOfInput)
{
    ScamValue args = readString("()");
    bool result = argsToParms(args, "test");
    EXPECT_TRUE(result);
}

TEST_F(ParameterTest, ArgToParmsTooFew)
{
    ObjectParameter parm1;
    ObjectParameter parm2;

    ScamValue args = readString("(1)");
    bool result = argsToParms(args, "test", parm1, parm2);
    EXPECT_FALSE(result);

    EXPECT_TRUE(parm1.valid);
    expectInteger(parm1.value, 1, "1", true);

    EXPECT_FALSE(parm2.valid);
}

TEST_F(ParameterTest, ArgToParmsTooMany)
{
    ObjectParameter parm1;
    ObjectParameter parm2;

    ScamValue args = readString("(1 2 3)");
    bool result = argsToParms(args, "test", parm1, parm2);
    EXPECT_FALSE(result);

    EXPECT_TRUE(parm1.valid);
    expectInteger(parm1.value, 1, "1", true);

    EXPECT_TRUE(parm2.valid);
    expectInteger(parm2.value, 2, "2", true);
}

TEST_F(ParameterTest, ArgToParmsJustRight)
{
    ObjectParameter parm1;
    ObjectParameter parm2;

    ScamValue args = readString("(1 2)");
    bool result = argsToParms(args, "test", parm1, parm2);
    EXPECT_TRUE(result);

    EXPECT_TRUE(parm1.valid);
    expectInteger(parm1.value, 1, "1", true);

    EXPECT_TRUE(parm2.valid);
    expectInteger(parm2.value, 2, "2", true);
}
