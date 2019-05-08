#include "TestBase.hpp"

#include "SampleManagedObject.hpp"
#include "TestHook.hpp"

#include "Extractor.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ScamExprAll.hpp"
#include "input/ClassDefParser.hpp"
#include "util/MemoryManager.hpp"

#include <iostream>

#include "util/DebugTrace.hpp"

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
        ScamBoolean * cut1 = mm.make<ScamBoolean>(val);
        ScamBoolean * cut2 = mm.make<ScamBoolean>(val);

        expectBoolean(cut1, val, rep);
        expectBoolean(cut2, val, rep);

        expectNonManaged(cut1, cut2);
    }

    void expectManaged(ExprHandle cut1,
                       ExprHandle cut2,
                       size_t count = 2)
    {
        EXPECT_NE(cut1, cut2);
        EXPECT_EQ(count, mm.getCreateCount());
        EXPECT_EQ(count, mm.getCurrentCount());
    }

    void expectNonManaged(ExprHandle cut1, ExprHandle cut2)
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
 *  the ExprHandleobjects.
 *
 * First test the primitive types.
 *
 *****************************************************************
 */
TEST_F(MemoryTest, TestScamNull)
{
    ScamNull * cut1 = mm.make<ScamNull>();
    ScamNull * cut2 = mm.make<ScamNull>();

    expectNull(cut1);
    expectNull(cut2);

    expectNonManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamNil)
{
    ScamNil * cut1 = mm.make<ScamNil>();
    ScamNil * cut2 = mm.make<ScamNil>();

    expectNil(cut1);
    expectNil(cut2);

    expectNonManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamBoolean)
{
    testBoolean(true,  "#t");
    testBoolean(false, "#f");
}

TEST_F(MemoryTest, TestScamCharacter)
{
    const char val { 'a' };
    const string repr { "#\\a" };

    ScamCharacter * cut1 = mm.make<ScamCharacter>(repr);
    ScamCharacter * cut2 = mm.make<ScamCharacter>(repr);

    expectChar(cut1, val, repr);
    expectChar(cut2, val, repr);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamInteger)
{
    const int value { 1 };
    const string repr { "1" };

    ScamInteger * cut1 = mm.make<ScamInteger>(value, true);
    ScamInteger * cut2 = mm.make<ScamInteger>(value, true);

    expectInteger(cut1, value, repr);
    expectInteger(cut2, value, repr);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamReal)
{
    const float value { 2.5 };
    const string repr { "2.5" };

    ScamReal * cut1 = mm.make<ScamReal>(value, false);
    ScamReal * cut2 = mm.make<ScamReal>(value, false);

    expectReal(cut1, value, repr);
    expectReal(cut2, value, repr);

    expectManaged(cut1, cut2);
}


TEST_F(MemoryTest, TestScamString)
{
    const string value { "my test string" };

    ScamString * cut1 = mm.make<ScamString>(value);
    ScamString * cut2 = mm.make<ScamString>(value);

    expectString(cut1, value);
    expectString(cut2, value);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamSymbol)
{
    const string value { "my test string" };

    ScamSymbol * cut1 = mm.make<ScamSymbol>(value);
    ScamSymbol * cut2 = mm.make<ScamSymbol>(value);

    expectSymbol(cut1, value);
    expectSymbol(cut2, value);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamKeyword)
{
    const string value { ":aKeyword" };

    ScamKeyword * cut1 = mm.make<ScamKeyword>(value);
    ScamKeyword * cut2 = mm.make<ScamKeyword>(value);

    expectKeyword(cut1, value);
    expectKeyword(cut2, value);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamError)
{
    const char * value { "I don't know what went wrong" };

    ScamError * cut1 = mm.make<ScamError>(value);
    ScamError * cut2 = mm.make<ScamError>(value);

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

TEST_F(MemoryTest, TestScamCons)
{
    ExprHandle car = mm.make<ScamInteger>(1, true);
    ExprHandle cdr = mm.make<ScamInteger>(2, true);
    ScamCons * cons1 = mm.make<ScamCons>(car, cdr);

    cons1->mark();
    expectMarked(true, cons1, car, cdr);
    EXPECT_TRUE(cons1->isMarked());
    EXPECT_TRUE(car->isMarked());
    EXPECT_TRUE(cdr->isMarked());
}

TEST_F(MemoryTest, TestScamDict)
{
    ScamDict * dict1 = mm.make<ScamDict>();
    ExprHandle key1 = mm.make<ScamKeyword>(":key1");
    ExprHandle key2 = mm.make<ScamKeyword>(":key2");
    ExprHandle val1 = mm.make<ScamInteger>(1, true);
    ExprHandle val2 = mm.make<ScamInteger>(2, true);
    ExprHandle val3 = mm.make<ScamInteger>(3, true);

    dict1->put(key1, val1);
    dict1->put(key2, val2);
    dict1->put(key1, val3);

    dict1->mark();
    expectMarked(true, dict1, key1, key2, val2, val3);
    expectMarked(false, val1);
}

TEST_F(MemoryTest, TestScamVector)
{
    ExprHandle val1 = mm.make<ScamInteger>(1, true);
    ExprHandle val2 = mm.make<ScamInteger>(2, true);
    ExprHandle val3 = mm.make<ScamInteger>(3, true);

    ExprVec elts;
    elts.push_back(val1);
    elts.push_back(val2);
    ScamVector * vec2 = mm.make<ScamVector>(elts);

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

TEST_F(MemoryTest, TestScamClosure)
{
    // (() (+ a b))
    ScamSymbol * symPlus = ExpressionFactory::makeSymbol("+");
    ScamSymbol * symA    = ExpressionFactory::makeSymbol("a");
    ScamSymbol * symB    = ExpressionFactory::makeSymbol("b");
    ExprHandle formals   = ExpressionFactory::makeList(symA, symB);
    ExprHandle aForm     = ExpressionFactory::makeList(symPlus, symA, symB);
    ExprHandle forms     = ExpressionFactory::makeList(formals, aForm);
    Env * env = standardMemoryManager.make<Env>();

    LambdaParser * lambda = mm.make<LambdaParser>();
    ASSERT_TRUE(lambda->accept(forms));

    ScamClosure * closure = mm.make<ScamClosure>(lambda, env);

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

TEST_F(MemoryTest, TestScamClass)
{
    ScamSymbol * base    = ExpressionFactory::makeSymbol("Root");

    ExprHandle vars    = ExpressionFactory::makeNil();

    ScamSymbol * symPlus = ExpressionFactory::makeSymbol("+");
    ScamSymbol * symA    = ExpressionFactory::makeSymbol("a");
    ScamSymbol * symB    = ExpressionFactory::makeSymbol("b");

    ScamSymbol * meth    = ExpressionFactory::makeSymbol("method");

    ExprHandle formals = ExpressionFactory::makeList(symA, symB);

    ExprHandle aForm   = ExpressionFactory::makeList(symPlus, symA, symB);
    ExprHandle func    = ExpressionFactory::makeList(meth, formals, aForm);
    ExprHandle def     = ExpressionFactory::makeList(base, vars, func);

    Env * env = mm.make<Env>();

    ClassDefParser * parser = mm.make<ClassDefParser>();
    ASSERT_TRUE(parser->accept(def));

    ScamClass * cls = mm.make<ScamClass>(parser, env);

    cls->mark();
    expectMarked(true,
                 cls, parser, env, base, vars,
                 symB, symA, aForm, symPlus, def, func);
}

TEST_F(MemoryTest, TestScamInstance)
{
    ScamSymbol * symPlus = ExpressionFactory::makeSymbol("+");
    ScamSymbol * symA    = ExpressionFactory::makeSymbol("a");
    ScamSymbol * symB    = ExpressionFactory::makeSymbol("b");

    ExprHandle name    = ExpressionFactory::makeSymbol("f");
    ExprHandle symQ    = ExpressionFactory::makeSymbol("q");
    ExprHandle args    = ExpressionFactory::makeList(symQ);
    ExprHandle aForm   = ExpressionFactory::makeList(symPlus, symA, symB, symQ);
    ScamSymbol * nom     = ExpressionFactory::makeSymbol("Notre Dame");
    ExprHandle vars    = ExpressionFactory::makeList(symA, symB);
    ExprHandle fun1    = ExpressionFactory::makeList(name, args, aForm);

    ExprHandle classDef = ExpressionFactory::makeList(nom, vars, fun1);

    ClassDefParser * def = mm.make<ClassDefParser>();
    ASSERT_TRUE(def->accept(classDef));

    Env * env = standardMemoryManager.make<Env>();
    ScamClass * cls = mm.make<ScamClass>(def, env);
    ScamInstance * instance = mm.make<ScamInstance>(cls, env);

    instance->mark();
    expectMarked(false, fun1, vars, name, nom);
    expectMarked(true, instance, env, symPlus, symA, symB, symQ, aForm, args);
}

TEST_F(MemoryTest, TestScamContinuation)
{
    Continuation     * original = mm.make<Continuation>("Test");
    ScamContinuation * wrapper  = mm.make<ScamContinuation>(original);

    wrapper->mark();
    expectMarked(true, wrapper, original);
}

TEST_F(MemoryTest, TestExtractor)
{
    Continuation * cont = mm.make<Extractor>();
    ExprHandle expr = mm.make<ScamKeyword>(":best");

    cont->run(expr);
    cont->mark();
    expectMarked(true, cont, expr);
}

TEST_F(MemoryTest, TestEnv)
{
    Env * top = standardMemoryManager.make<Env>();
    Env * env = top->extend();
    ScamSymbol * key = ExpressionFactory::makeSymbol("f");
    ExprHandle val = ExpressionFactory::makeInteger(333, true);

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
