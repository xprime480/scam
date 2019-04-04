
#include "util/MemoryManager.hpp"

#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class TestCollectible : public ManagedObject
{
public:
    ~TestCollectible()
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

    static TestCollectible * makeInstance()
    {
        return new TestCollectible();
    }

    static TestCollectible * makeInstance(int value)
    {
        return new TestCollectible(value);
    }

    static TestCollectible * makeInstance(TestCollectible * proxy)
    {
        return new TestCollectible(proxy);
    }

    int getValue() const
    {
        if ( proxy ) {
            return proxy->getValue();
        }
        return value;
    }

private:
    TestCollectible()
        : value(7)
        , proxy(nullptr)
    {
    }

    explicit TestCollectible(int value)
        : value(value)
        , proxy(nullptr)
    {
    }

    explicit TestCollectible(TestCollectible * proxy)
        : value(0)
        , proxy(proxy)
    {
    }

    int value;
    TestCollectible * proxy;
};

class MemoryTest //: public ExpressionTestBase
 : public ::testing::Test
{
};

TEST_F(MemoryTest, MarkTest)
{
    MemoryManager mm;
    TestCollectible * proxy = mm.make<TestCollectible>(33);
    ASSERT_NE(nullptr, proxy);

    TestCollectible * cut = mm.make<TestCollectible>(proxy);
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
    MemoryManager mm;
    TestCollectible * cut = mm.make<TestCollectible>();
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(7, cut->getValue());
    EXPECT_EQ(1, mm.getCreateCount());
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, CreateTestValue)
{
    MemoryManager mm;
    TestCollectible * cut = mm.make<TestCollectible>(-1);
    ASSERT_NE(nullptr, cut);
    EXPECT_EQ(-1, cut->getValue());
    EXPECT_EQ(1, mm.getCreateCount());
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestGCNotNeeded)
{
    MemoryManager mm;
    TestCollectible * cut = mm.make<TestCollectible>();
    EXPECT_EQ(7, cut->getValue());

    EXPECT_EQ(1, mm.getCurrentCount());
    mm.gc();
    EXPECT_EQ(1, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestGCNoRoots)
{
    MemoryManager mm(2);
    TestCollectible * cut = mm.make<TestCollectible>();
    cut = mm.make<TestCollectible>();
    cut = mm.make<TestCollectible>();
    cut = mm.make<TestCollectible>();
    EXPECT_EQ(7, cut->getValue());

    EXPECT_EQ(4, mm.getCurrentCount());
    mm.gc();
    EXPECT_EQ(0, mm.getCurrentCount());
}

TEST_F(MemoryTest, GCTestOneRoot)
{
    MemoryManager mm(2);
    TestCollectible * cut = mm.make<TestCollectible>();
    cut = mm.make<TestCollectible>();
    cut = mm.make<TestCollectible>();
    cut = mm.make<TestCollectible>();

    EXPECT_EQ(4, mm.getCurrentCount());

    std::function<void(void)> hook = [&cut](){ cut->mark(); };
    mm.addHook(hook);

    mm.gc();

    EXPECT_EQ(1, mm.getCurrentCount());
    EXPECT_FALSE(cut->isMarked());
}

TEST_F(MemoryTest, GCTestWithProxy)
{
    MemoryManager mm(2);
    TestCollectible * proxy = mm.make<TestCollectible>(33);
    ASSERT_NE(nullptr, proxy);

    mm.make<TestCollectible>();
    mm.make<TestCollectible>();
    TestCollectible * cut = mm.make<TestCollectible>(proxy);
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
