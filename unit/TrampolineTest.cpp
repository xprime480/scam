
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "util/MemoryManager.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;

namespace
{
    WorkQueue queue;
}

class TestWorker : public Worker
{
protected:
    TestWorker(size_t * counter, char const *name)
        : Worker(name)
        , counter(counter)
    {
    }

public:
    void run() override
    {
        Worker::run();
        ++*counter;
    }

protected:
    size_t * counter;
};


class OneShotWorker : public TestWorker
{
private:
    friend class scam::MemoryManager;
    OneShotWorker(size_t * counter)
        : TestWorker(counter, "OneShotWorker")
    {
    }

    static OneShotWorker * makeInstance(size_t * counter)
    {
        return new OneShotWorker(counter);
    }

public:
    void run() override
    {
        TestWorker::run();
        // cout << "OneShotWorker here\n";
    }
};

class Countdown : public TestWorker
{
private:
    friend class scam::MemoryManager;
    Countdown(size_t n, size_t * counter)
        : TestWorker(counter, "CountDown")
        , n(n)
    {
    }

    static Countdown * makeInstance(size_t n, size_t * counter)
    {
        return new Countdown(n, counter);
    }

public:
    void run() override
    {
        TestWorker::run();
        if ( 0 == n ) {
            //            cout << "go!!\n";
        }
        else {
            //            cout << n << "... ";

            Worker * next =
                standardMemoryManager.make<Countdown>(n-1, counter);
            queue.put(next);
        }
    }

private:
    size_t n;
};

TEST(TrampolineTest, SimpleTest)
{
    MemoryManager & mm = scam::standardMemoryManager;

    size_t exec1{ 0 };
    Worker * count = mm.make<Countdown>(3, &exec1);
    queue.put(count);

    size_t exec2{ 0 };
    Worker * start = mm.make<OneShotWorker>(&exec2);
    queue.put(start);

    Trampoline(queue);

    EXPECT_EQ(4, exec1);
    EXPECT_EQ(1, exec2);
}
