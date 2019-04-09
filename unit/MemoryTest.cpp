
#include "TestBase.hpp"

#include "expr/ScamExprAll.hpp"
#include "util/MemoryManager.hpp"
#include "ScamException.hpp"
#include "Extractor.hpp"

#include <iostream>


using namespace std;
using namespace scam;

/**
 * MemoryTest
 *
 * basic test class for memory tests
 */
class MemoryTest : public TestBase
{

protected:
    MemoryTest()
    {
    }

    void SetUp() override
    {
        TestBase::SetUp();
        mm.reset();
        mm.setSize(2u);
    }

    void testBoolean(bool val, string const & rep)
    {
        ScamBoolean * cut1 = mm.make<ScamBoolean>(val);
        ScamBoolean * cut2 = mm.make<ScamBoolean>(val);

        expectBoolean(cut1, val, rep);
        expectBoolean(cut2, val, rep);

        expectNonManaged(cut1, cut2);
    }

    void expectManaged(ScamExpr const * cut1,
                       ScamExpr const * cut2,
                       size_t count = 2)
    {
        EXPECT_NE(cut1, cut2);
        EXPECT_EQ(count, mm.getCreateCount());
        EXPECT_EQ(count, mm.getCurrentCount());
    }

    void expectNonManaged(ScamExpr const * cut1, ScamExpr const * cut2)
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

/*
 * ManagedObjectTest
 *
 * a subclass of ManagedObject to exercise the basic functionality
 */
class ManagedObjectTest : public ManagedObject
{
public:
    ~ManagedObjectTest()
    {
    }

    void mark() const override
    {
        if ( ! isMarked() ) {
            ManagedObject::mark();
            if ( proxy ) {
                proxy->mark();
            }
        }
    }

    static ManagedObjectTest * makeInstance()
    {
        return new ManagedObjectTest();
    }

    static ManagedObjectTest * makeInstance(int value)
    {
        return new ManagedObjectTest(value);
    }

    static ManagedObjectTest * makeInstance(ManagedObjectTest * proxy)
    {
        return new ManagedObjectTest(proxy);
    }

    int getValue() const
    {
        if ( proxy ) {
            return proxy->getValue();
        }
        return value;
    }

private:
    ManagedObjectTest()
        : value(7)
        , proxy(nullptr)
    {
    }

    explicit ManagedObjectTest(int value)
        : value(value)
        , proxy(nullptr)
    {
    }

    explicit ManagedObjectTest(ManagedObjectTest * proxy)
        : value(0)
        , proxy(proxy)
    {
    }

    int value;
    ManagedObjectTest * proxy;
};

/*****************************************************************
 * The first set of tests test the basic functionality with
 *  the ManagedObjectTest class
 *****************************************************************
 */

TEST_F(MemoryTest, MarkTest)
{
    ManagedObjectTest * proxy = mm.make<ManagedObjectTest>(33);
    ASSERT_NE(nullptr, proxy);

    ManagedObjectTest * cut = mm.make<ManagedObjectTest>(proxy);
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
    ManagedObjectTest * cut = mm.make<ManagedObjectTest>();
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(7, cut->getValue());
    EXPECT_EQ(1, mm.getCreateCount());
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, CreateTestValue)
{
    ManagedObjectTest * cut = mm.make<ManagedObjectTest>(-1);
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(-1, cut->getValue());
    EXPECT_EQ(1, mm.getCreateCount());
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestGCNotNeeded)
{
    ManagedObjectTest * cut = mm.make<ManagedObjectTest>();
    EXPECT_EQ(7, cut->getValue());

    EXPECT_EQ(1, mm.getCurrentCount());
    mm.gc();
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestGCNoRoots)
{
    ManagedObjectTest * cut = mm.make<ManagedObjectTest>();
    cut = mm.make<ManagedObjectTest>();
    cut = mm.make<ManagedObjectTest>();
    cut = mm.make<ManagedObjectTest>();
    EXPECT_EQ(7, cut->getValue());

    EXPECT_EQ(4, mm.getCurrentCount());
    mm.gc();
    EXPECT_EQ(0, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestOneRoot)
{
    (void) mm.make<ManagedObjectTest>();
    (void) mm.make<ManagedObjectTest>();
    (void) mm.make<ManagedObjectTest>();
    ManagedObjectTest * cut = mm.make<ManagedObjectTest>();

    EXPECT_EQ(4, mm.getCurrentCount());

    std::function<void(void)> hook = [&cut](){ cut->mark(); };
    mm.addHook(hook);

    mm.gc();

    EXPECT_EQ(1, mm.getCurrentCount());
    expectMarked(false, cut);
}

TEST_F(MemoryTest, GCTestWithProxy)
{
    ManagedObjectTest * proxy = mm.make<ManagedObjectTest>(33);
    ASSERT_NE(nullptr, proxy);

    (void) mm.make<ManagedObjectTest>();
    (void) mm.make<ManagedObjectTest>();
    ManagedObjectTest * cut = mm.make<ManagedObjectTest>(proxy);

    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(33, cut->getValue());
    EXPECT_EQ(4, mm.getCurrentCount());

    std::function<void(void)> hook = [&cut](){ cut->mark(); };
    mm.addHook(hook);
    mm.gc();

    EXPECT_EQ(2, mm.getCurrentCount());
    expectMarked(false, cut, proxy);
    EXPECT_EQ(33, cut->getValue());
}

/*****************************************************************
 * The second set of tests test the basic functionality with
 *  the ScamExpr objects.
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
    const string repr { "#\\a" };

    ScamCharacter * cut1 = mm.make<ScamCharacter>(repr);
    ScamCharacter * cut2 = mm.make<ScamCharacter>(repr);

    expectChar(cut1, 'a', repr);
    expectChar(cut2, 'a', repr);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamInteger)
{
    const int value { 1 };
    const string repr { "1" };

    ScamInteger * cut1 = mm.make<ScamInteger>(value);
    ScamInteger * cut2 = mm.make<ScamInteger>(value);

    expectInteger(cut1, value, repr);
    expectInteger(cut2, value, repr);

    expectManaged(cut1, cut2);
}

TEST_F(MemoryTest, TestScamFloat)
{
    const float value { 2.5 };
    const string repr { "2.5" };

    ScamFloat * cut1 = mm.make<ScamFloat>(value);
    ScamFloat * cut2 = mm.make<ScamFloat>(value);

    expectFloat(cut1, value, repr);
    expectFloat(cut2, value, repr);

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
    ScamExpr * car = mm.make<ScamInteger>(1);
    ScamExpr * cdr = mm.make<ScamInteger>(2);
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
    ScamExpr * key1 = mm.make<ScamKeyword>(":key1");
    ScamExpr * key2 = mm.make<ScamKeyword>(":key2");
    ScamExpr * val1 = mm.make<ScamInteger>(1);
    ScamExpr * val2 = mm.make<ScamInteger>(2);
    ScamExpr * val3 = mm.make<ScamInteger>(3);

    dict1->put(key1, val1);
    dict1->put(key2, val2);
    dict1->put(key1, val3);

    dict1->mark();
    expectMarked(true, dict1, key1, key2, val2, val3);
    expectMarked(false, val1);
}

TEST_F(MemoryTest, TestScamVector)
{
    ScamExpr * val1 = mm.make<ScamInteger>(1);
    ScamExpr * val2 = mm.make<ScamInteger>(2);
    ScamExpr * val3 = mm.make<ScamInteger>(3);

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
    ScamSymbol * symPlus = ExpressionFactory::makeSymbol("+");
    ScamSymbol * symA    = ExpressionFactory::makeSymbol("a");
    ScamSymbol * symB    = ExpressionFactory::makeSymbol("b");
    ScamExpr   * formals = ExpressionFactory::makeList(symA, symB);
    ScamExpr   * aForm   = ExpressionFactory::makeList(symPlus, symA, symB);
    ScamExpr   * forms   = ExpressionFactory::makeList(aForm);
    Env env;

    ScamClosure * closure = mm.make<ScamClosure>(formals, forms, env);

    closure->mark();
    expectMarked(true, closure, forms, aForm, formals, symB, symA, symPlus);
}

TEST_F(MemoryTest, TestScamClass)
{
    ScamNil    * base    = ExpressionFactory::makeNil();
    ScamSymbol * symPlus = ExpressionFactory::makeSymbol("+");
    ScamSymbol * symA    = ExpressionFactory::makeSymbol("a");
    ScamSymbol * symB    = ExpressionFactory::makeSymbol("b");
    ScamExpr   * vars    = ExpressionFactory::makeList(symA, symB);
    ScamExpr   * aForm   = ExpressionFactory::makeList(symPlus, symA, symB);
    ScamExpr   * funs    = ExpressionFactory::makeList(aForm);
    Env env;

    ScamClass * cls = mm.make<ScamClass>(base, vars, funs, env);

    cls->mark();
    expectMarked(true, cls, funs, aForm, vars, symB, symA, symPlus, base);
}

TEST_F(MemoryTest, TestScamInstance)
{
    ScamSymbol * symPlus = ExpressionFactory::makeSymbol("+");
    ScamSymbol * symA    = ExpressionFactory::makeSymbol("a");
    ScamSymbol * symB    = ExpressionFactory::makeSymbol("b");
    ScamExpr   * vars    = ExpressionFactory::makeList(symA, symB);

    ScamExpr   * name    = ExpressionFactory::makeSymbol("f");
    ScamExpr   * symQ    = ExpressionFactory::makeSymbol("q");
    ScamExpr   * args    = ExpressionFactory::makeList(symQ);
    ScamExpr   * aForm   = ExpressionFactory::makeList(symPlus, symA, symB, symQ);
    ScamExpr   * fun1    = ExpressionFactory::makeList(name, args, aForm);
    ScamExpr   * funs    = ExpressionFactory::makeList(fun1);

    Env env;

    ScamInstance * instance = mm.make<ScamInstance>(vars, funs, env);

    instance->mark();
    expectMarked(true, instance);
    expectMarked(false, funs, fun1, vars);

    // change false to true in the following after we make
    // environments part of managed memory
    //
    expectMarked(false, aForm, args, symQ, name, symB, symA, symPlus);
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
    ScamExpr     * expr = mm.make<ScamKeyword>(":best");

    cont->run(expr);
    cont->mark();
    expectMarked(true, cont, expr);
}
