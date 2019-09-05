#include "TestBase.hpp"

#include "SampleManagedObject.hpp"
#include "TestHook.hpp"

#include "Extractor.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "env/Env.hpp"
#include "util/ClassDef.hpp"
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

    void expectManaged(ScamValue cut1, ScamValue cut2, size_t count = 2)
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

    void expectCached(ScamValue cut1, ScamValue cut2)
    {
        EXPECT_EQ(cut1, cut2);
        EXPECT_EQ(1, mm.getCreateCount());
        EXPECT_EQ(1, mm.getCurrentCount());
    }

    void expectMarked(bool value) {}

    template <typename... Ts>
    void expectMarked(bool value, ManagedObject * obj, Ts && ...rest)
    {
        ScamValue t = dynamic_cast<ScamValue>(obj);
        EXPECT_EQ(value, obj->isMarked()) << (t ? writeValue(t) : "");
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
    mm.gc(true);
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
    mm.gc(true);

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
    mm.gc(true);

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
    ScamValue cut1 = makeNothing();
    ScamValue cut2 = makeNothing();

    expectNothing(cut1);
    expectNothing(cut2);

    expectNonManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestNil)
{
    ScamValue cut1 = makeNull();
    ScamValue cut2 = makeNull();

    expectNull(cut1);
    expectNull(cut2);

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

    expectCached(cut1, cut2);
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
    Env * env = mm.make<Env>();

    LambdaDef lambda;
    ScamValue rv = lambda.transform(forms);
    ASSERT_TRUE(isNull(rv));

    ScamValue closure = makeClosure(lambda, env);

    closure->mark();
    expectMarked(false, forms, formals);
    expectMarked(true, closure, env, aForm, symB, symA, symPlus);
}

TEST_F(MemoryTest, TestClass)
{
    ScamValue base = makeSymbol("Root");
    ScamValue vars = makeNull();

    ScamValue symPlus = makeSymbol("+");
    ScamValue symA    = makeSymbol("a");
    ScamValue symB    = makeSymbol("b");
    ScamValue meth    = makeSymbol("method");

    ScamValue formals = makeList(symA, symB);

    ScamValue aForm   = makeList(symPlus, symA, symB);
    ScamValue func    = makeList(meth, formals, aForm);
    ScamValue args    = makeList(base, vars, func);

    Env * env = mm.make<Env>();

    ClassDef def;
    expectNull(def.transform(args));

    ScamValue cls = makeClass(def, env);

    cls->mark();
    expectMarked(false, args, func);
    expectMarked(true, cls, env, base, vars, symB, symA, aForm, symPlus);
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

    ClassDef def;
    expectNull(def.transform(classDef));

    Env * env = mm.make<Env>();
    ScamValue cls = makeClass(def, env);
    ScamValue instance = makeClassInstance(cls, env);
    expectInstance(instance);

    instance->mark();
    expectMarked(false, fun1, vars, name, nom, args);
    expectMarked(true, instance, env, symPlus, symA, symB, symQ, aForm);
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

    cont->handleValue(expr);
    cont->mark();
    expectMarked(true, cont, expr);
}

TEST_F(MemoryTest, TestEnv)
{
    Env * top = mm.make<Env>();
    Env * env = top->extend();
    ScamValue key = makeSymbol("f");
    ScamValue val = makeInteger(333, true);

    ScamValue test = top->put(key, val);
    ASSERT_TRUE(isNothing(test));
    env->mark();
    expectMarked(true, top, env, val);
    expectMarked(false, key);
}

TEST_F(MemoryTest, TestWorkQueue)
{
    Worker * worker = mm.make<Worker>("Test");
    WorkQueue queue;

    queue.put(worker);

    queue.mark();
    expectMarked(true, worker);
}
