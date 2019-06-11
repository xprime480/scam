#include "TestBase.hpp"

#include "SampleManagedObject.hpp"
#include "TestHook.hpp"

#include "Env.hpp"
#include "Extractor.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "input/ClassDefParser.hpp"
#include "input/LambdaParser.hpp"
#include "util/MemoryManager.hpp"

#include <iostream>

using namespace std;
using namespace scam;
using namespace scam::test_impl;

/**
 * MemoryTest
 *
 * basic test class for memory tests
 */
class MemoryTest : public TestBase
{
protected:
    TestHook hook;

    MemoryTest()
    {
    }

    void SetUp() override
    {
        TestBase::SetUp();
        mm.reset();
        mm.setSize(2u);
        mm.addHook(&hook);
    }

    void TearDown() override
    {
        mm.removeHook(&hook);
        TestBase::TearDown();
    }

    void testBoolean(bool val, string const & rep)
    {
        ScamValue cut1 = makeBoolean(val);
        ScamValue cut2 = makeBoolean(val);

        expectBoolean(cut1, val, rep);
        expectBoolean(cut2, val, rep);

        expectNonManaged(cut1, cut2);
    }

    void expectManaged(ScamValue cut1,
                       ScamValue cut2,
                       size_t count = 2)
    {
        EXPECT_NE(cut1, cut2);
        EXPECT_EQ(count, mm.getCreateCount());
        EXPECT_EQ(count, mm.getCurrentCount());
    }

    void expectNonManaged(ScamValue cut1, ScamValue cut2)
    {
        EXPECT_EQ(cut1, cut2);
        EXPECT_EQ(0, mm.getCreateCount());
        EXPECT_EQ(0, mm.getCurrentCount());
    }

    void expectMarked(bool value) {}

    template <typename... Ts>
    void expectMarked(bool value, ManagedObject * obj, Ts && ...rest)
    {
        EXPECT_EQ(value, obj->isMarked());
        expectMarked(value, rest...);
    }
};

/*****************************************************************
 * The first set of tests test the basic functionality with
 *  the SampleManagedObject class
 *****************************************************************
 */

TEST_F(MemoryTest, MarkTest)
{
    SampleManagedObject * proxy = mm.make<SampleManagedObject>(33);
    ASSERT_NE(nullptr, proxy);

    SampleManagedObject * cut = mm.make<SampleManagedObject>(proxy);
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(33, cut->getValue());

    expectMarked(false, cut, proxy);

    cut->mark();
    expectMarked(true, cut, proxy);

    cut->unmark();
    expectMarked(true, proxy);
    expectMarked(false, cut);
}

TEST_F(MemoryTest, CreateTestDefault)
{
    SampleManagedObject * cut = mm.make<SampleManagedObject>();
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(7, cut->getValue());
    EXPECT_EQ(1, mm.getCreateCount());
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, CreateTestValue)
{
    SampleManagedObject * cut = mm.make<SampleManagedObject>(-1);
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(-1, cut->getValue());
    EXPECT_EQ(1, mm.getCreateCount());
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestGCNotNeeded)
{
    SampleManagedObject * cut = mm.make<SampleManagedObject>();
    EXPECT_EQ(7, cut->getValue());

    EXPECT_EQ(1, mm.getCurrentCount());
    mm.gc();
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestGCNoRoots)
{
    SampleManagedObject * cut = mm.make<SampleManagedObject>();
    cut = mm.make<SampleManagedObject>();
    cut = mm.make<SampleManagedObject>();
    cut = mm.make<SampleManagedObject>();
    EXPECT_EQ(7, cut->getValue());

    EXPECT_EQ(4, mm.getCurrentCount());
    mm.gc();
    EXPECT_EQ(0, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestOneRoot)
{
    (void) mm.make<SampleManagedObject>();
    (void) mm.make<SampleManagedObject>();
    (void) mm.make<SampleManagedObject>();
    SampleManagedObject * cut = mm.make<SampleManagedObject>();

    EXPECT_EQ(4, mm.getCurrentCount());

    hook.addRoot(cut);
    mm.gc();

    EXPECT_EQ(1, mm.getCurrentCount());
    expectMarked(false, cut);
}

TEST_F(MemoryTest, GCTestWithProxy)
{
    SampleManagedObject * proxy = mm.make<SampleManagedObject>(33);
    ASSERT_NE(nullptr, proxy);

    (void) mm.make<SampleManagedObject>();
    (void) mm.make<SampleManagedObject>();
    SampleManagedObject * cut = mm.make<SampleManagedObject>(proxy);

    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(33, cut->getValue());
    EXPECT_EQ(4, mm.getCurrentCount());

    hook.addRoot(cut);
    mm.gc();

    EXPECT_EQ(2, mm.getCurrentCount());
    expectMarked(false, cut, proxy);
    EXPECT_EQ(33, cut->getValue());
}

/*****************************************************************
 * The second set of tests test the basic functionality with
 *  the ScamValueobjects.
 *
 * First test the primitive types.
 *
 *****************************************************************
 */
TEST_F(MemoryTest, TestNull)
{
    ScamValue cut1 = makeNull();
    ScamValue cut2 = makeNull();

    expectNull(cut1);
    expectNull(cut2);

    expectNonManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestNil)
{
    ScamValue cut1 = makeNil();
    ScamValue cut2 = makeNil();

    expectNil(cut1);
    expectNil(cut2);

    expectNonManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestBoolean)
{
    testBoolean(true,  "#t");
    testBoolean(false, "#f");
}

TEST_F(MemoryTest, TestCharacter)
{
    const char val { 'a' };
    const string repr { "#\\a" };

    ScamValue cut1 = makeCharacter(val);
    ScamValue cut2 = makeCharacter(val);

    expectChar(cut1, val, repr);
    expectChar(cut2, val, repr);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamInteger)
{
    const int value { 1 };
    const string repr { "1" };

    ScamValue cut1 = makeInteger(value, true);
    ScamValue cut2 = makeInteger(value, true);

    expectInteger(cut1, value, repr, true);
    expectInteger(cut2, value, repr, true);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamReal)
{
    const float value { 2.5 };
    const string repr { "2.5" };

    ScamValue cut1 = makeReal(value, false);
    ScamValue cut2 = makeReal(value, false);

    expectReal(cut1, value, repr, false);
    expectReal(cut2, value, repr, false);

    expectManaged(cut1, cut2);
}


TEST_F(MemoryTest, TestString)
{
    const string value { "my test string" };
    const string repr { "\"my test string\"" };

    ScamValue cut1 = makeString(value);
    ScamValue cut2 = makeString(value);

    expectString(cut1, repr);
    expectString(cut2, repr);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestSymbol)
{
    const string value { "my test string" };

    ScamValue cut1 = makeSymbol(value);
    ScamValue cut2 = makeSymbol(value);

    expectSymbol(cut1, value);
    expectSymbol(cut2, value);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestKeyword)
{
    const string value { ":aKeyword" };

    ScamValue cut1 = makeKeyword(value);
    ScamValue cut2 = makeKeyword(value);

    expectKeyword(cut1, value);
    expectKeyword(cut2, value);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestError)
{
    const char * value { "I don't know what went wrong" };

    ScamValue cut1 = makeError(value);
    ScamValue cut2 = makeError(value);

    expectError(cut1, value);
    expectError(cut2, value);

    expectManaged(cut1, cut2);
}

/*****************************************************************
 *
 * Now test the composite types.
 *
 * The distinguishing feature is that composite types have
 * components that must be marked when the main object is marked.
 *
 *****************************************************************
 */

TEST_F(MemoryTest, TestPair)
{
    ScamValue car = makeInteger(1, true);
    ScamValue cdr = makeInteger(2, true);
    ScamValue pair1 = makePair(car, cdr);

    pair1->mark();
    expectMarked(true, pair1, car, cdr);
    EXPECT_TRUE(pair1->isMarked());
    EXPECT_TRUE(car->isMarked());
    EXPECT_TRUE(cdr->isMarked());
}

TEST_F(MemoryTest, TestDict)
{
    ScamValue dict1 = makeDict();
    ScamValue key1  = makeKeyword(":key1");
    ScamValue key2  = makeKeyword(":key2");
    ScamValue val1  = makeInteger(1, true);
    ScamValue val2  = makeInteger(2, true);
    ScamValue val3  = makeInteger(3, true);

    dictPut(dict1, key1, val1);
    dictPut(dict1, key2, val2);
    dictPut(dict1, key1, val3);

    dict1->mark();
    expectMarked(true, dict1, key1, key2, val2, val3);
    expectMarked(false, val1);
}

TEST_F(MemoryTest, TestVector)
{
    ScamValue val1 = makeInteger(1, true);
    ScamValue val2 = makeInteger(2, true);
    ScamValue val3 = makeInteger(3, true);

    ExprVec elts;
    elts.push_back(val1);
    elts.push_back(val2);
    ScamValue vec2 = makeVector(elts);

    vec2->mark();
    expectMarked(true, vec2, val1, val2);
    expectMarked(false, val3);
}

/*****************************************************************
 *
 * Finally test the remaining types.
 *
 * These are composite types more complex than just "data
 * structures".
 *
 *****************************************************************
 */

TEST_F(MemoryTest, TestClosure)
{
    // (() (+ a b))
    ScamValue symPlus = makeSymbol("+");
    ScamValue symA    = makeSymbol("a");
    ScamValue symB    = makeSymbol("b");
    ScamValue formals = makeList(symA, symB);
    ScamValue aForm   = makeList(symPlus, symA, symB);
    ScamValue forms   = makeList(formals, aForm);
    Env * env = standardMemoryManager.make<Env>();

    LambdaParser * lambda = mm.make<LambdaParser>();
    ASSERT_TRUE(lambda->accept(forms));

    ScamValue closure = makeClosure(lambda, env);

    closure->mark();
    expectMarked(true, closure);
    expectMarked(true, env);
    expectMarked(true, forms);
    expectMarked(true, aForm);
    expectMarked(true, formals);
    expectMarked(true, symB);
    expectMarked(true, symA);
    expectMarked(true, symPlus);
}

TEST_F(MemoryTest, TestClass)
{
    ScamValue base = makeSymbol("Root");
    ScamValue vars = makeNil();

    ScamValue symPlus = makeSymbol("+");
    ScamValue symA    = makeSymbol("a");
    ScamValue symB    = makeSymbol("b");
    ScamValue meth    = makeSymbol("method");

    ScamValue formals = makeList(symA, symB);

    ScamValue aForm   = makeList(symPlus, symA, symB);
    ScamValue func    = makeList(meth, formals, aForm);
    ScamValue def     = makeList(base, vars, func);

    Env * env = mm.make<Env>();

    ClassDefParser * parser = mm.make<ClassDefParser>();
    ASSERT_TRUE(parser->accept(def));

    ScamValue cls = makeClass(parser, env);

    cls->mark();
    expectMarked(true,
                 cls, parser, env, base, vars,
                 symB, symA, aForm, symPlus, def, func);
}

TEST_F(MemoryTest, TestInstance)
{
    ScamValue symPlus = makeSymbol("+");
    ScamValue symA    = makeSymbol("a");
    ScamValue symB    = makeSymbol("b");

    ScamValue name    = makeSymbol("f");
    ScamValue symQ    = makeSymbol("q");
    ScamValue args    = makeList(symQ);
    ScamValue aForm   = makeList(symPlus, symA, symB, symQ);
    ScamValue nom     = makeSymbol("Notre Dame");
    ScamValue vars    = makeList(symA, symB);
    ScamValue fun1    = makeList(name, args, aForm);

    ScamValue classDef = makeList(nom, vars, fun1);

    ClassDefParser * def = mm.make<ClassDefParser>();
    ASSERT_TRUE(def->accept(classDef));

    Env * env = standardMemoryManager.make<Env>();
    ScamValue cls = makeClass(def, env);
    ScamValue instance = makeClassInstance(cls, env);

    instance->mark();
    expectMarked(false, fun1, vars, name, nom);
    expectMarked(true, instance, env, symPlus, symA, symB, symQ, aForm, args);
}

TEST_F(MemoryTest, TestContinuation)
{
    Continuation * original = mm.make<Continuation>("Test");
    ScamValue      wrapper  = makeContinuation(original);

    wrapper->mark();
    expectMarked(true, wrapper, original);
}

TEST_F(MemoryTest, TestExtractor)
{
    Continuation * cont = mm.make<Extractor>();
    ScamValue expr = makeKeyword(":best");

    cont->run(expr);
    cont->mark();
    expectMarked(true, cont, expr);
}

TEST_F(MemoryTest, TestEnv)
{
    Env * top = standardMemoryManager.make<Env>();
    Env * env = top->extend();
    ScamValue key = makeSymbol("f");
    ScamValue val = makeInteger(333, true);

    top->put(key, val);
    env->mark();
    expectMarked(true, top, env, val);
    expectMarked(false, key);
}

TEST_F(MemoryTest, TestWorkQueue)
{
    Worker * worker = standardMemoryManager.make<Worker>("Test");
    WorkQueue queue;

    queue.put(worker);

    queue.mark();
    expectMarked(true, worker);
}
