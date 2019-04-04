
#include "util/MemoryManager.hpp"

#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

/**
 * MemoryTest
 *
 * basic test class for memory tests
 */
class MemoryTest //: public ExpressionTestBase
 : public ::testing::Test
{

protected:
    MemoryTest()
      : mmLarge()
      , mmSmall(2)
    {
    }

  MemoryManager mmLarge;
  MemoryManager mmSmall;

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

    void mark() override
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
    MemoryManager & mm = mmLarge;

    ManagedObjectTest * proxy = mm.make<ManagedObjectTest>(33);
    ASSERT_NE(nullptr, proxy);

    ManagedObjectTest * cut = mm.make<ManagedObjectTest>(proxy);
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(33, cut->getValue());

    EXPECT_FALSE(proxy->isMarked());
    EXPECT_FALSE(cut->isMarked());

    cut->mark();

    EXPECT_TRUE(proxy->isMarked());
    EXPECT_TRUE(cut->isMarked());

    cut->unmark();

    EXPECT_TRUE(proxy->isMarked());
    EXPECT_FALSE(cut->isMarked());
}

TEST_F(MemoryTest, CreateTestDefault)
{
    MemoryManager &  mm = mmLarge;

    ManagedObjectTest * cut = mm.make<ManagedObjectTest>();
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(7, cut->getValue());
    EXPECT_EQ(1, mm.getCreateCount());
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, CreateTestValue)
{
    MemoryManager &  mm = mmLarge;

    ManagedObjectTest * cut = mm.make<ManagedObjectTest>(-1);
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(-1, cut->getValue());
    EXPECT_EQ(1, mm.getCreateCount());
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestGCNotNeeded)
{
    MemoryManager &  mm = mmLarge;

    ManagedObjectTest * cut = mm.make<ManagedObjectTest>();
    EXPECT_EQ(7, cut->getValue());

    EXPECT_EQ(1, mm.getCurrentCount());
    mm.gc();
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestGCNoRoots)
{
    MemoryManager &  mm = mmSmall;

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
    MemoryManager &  mm = mmSmall;

    ManagedObjectTest * cut = mm.make<ManagedObjectTest>();
    cut = mm.make<ManagedObjectTest>();
    cut = mm.make<ManagedObjectTest>();
    cut = mm.make<ManagedObjectTest>();

    EXPECT_EQ(4, mm.getCurrentCount());

    std::function<void(void)> hook = [&cut](){ cut->mark(); };
    mm.addHook(hook);

    mm.gc();

    EXPECT_EQ(1, mm.getCurrentCount());
    EXPECT_FALSE(cut->isMarked());
}

TEST_F(MemoryTest, GCTestWithProxy)
{
    MemoryManager &  mm = mmSmall;

    ManagedObjectTest * proxy = mm.make<ManagedObjectTest>(33);
    ASSERT_NE(nullptr, proxy);

    mm.make<ManagedObjectTest>();
    mm.make<ManagedObjectTest>();
    ManagedObjectTest * cut = mm.make<ManagedObjectTest>(proxy);
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(33, cut->getValue());

    EXPECT_EQ(4, mm.getCurrentCount());

    std::function<void(void)> hook = [&cut](){ cut->mark(); };
    mm.addHook(hook);

    mm.gc();

    EXPECT_EQ(2, mm.getCurrentCount());
    EXPECT_FALSE(cut->isMarked());
    EXPECT_FALSE(proxy->isMarked());
    EXPECT_EQ(33, cut->getValue());
}

/*****************************************************************
 * The second set of tests test the basic functionality with
 *  the ScamExpr objects
 *****************************************************************
 */

/**
TEST_F(MemoryTest, TestScamNil)
{
    MemoryManager &  mm = mmSmall;

    ManagedObjectTest * cut = mm.make<ScamNull>();
    ASSERT_NE(nullptr, cut);

    EXPECT_EQ(1, mm.getCreateCount());
    EXPECT_EQ(1, mm.getCurrentCount());
}
**/
