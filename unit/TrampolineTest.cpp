
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;

namespace
{
    WorkQueue queue;
}

class TestWorker : public Worker
{
public:
    TestWorker(size_t * counter, char const *name)
        : Worker(name)
        , counter(counter)
    {
    }

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
public:
    OneShotWorker(size_t * counter)
        : TestWorker(counter, "OneShotWorker")
    {
    }

    void run() override
    {
        TestWorker::run();
        // cout << "OneShotWorker here\n";
    }
};

class Countdown : public TestWorker
{
public:
    Countdown(size_t n, size_t * counter)
        : TestWorker(counter, "CountDown")
        , n(n)
    {
    }

    void run() override
    {
        TestWorker::run();
        if ( 0 == n ) {
            //            cout << "go!!\n";
        }
        else {
            //            cout << n << "... ";

            WorkerHandle next = std::make_shared<Countdown>(n-1, counter);
            queue.put(next);
        }
    }

private:
    size_t n;
};

TEST(TrampolineTest, SimpleTest)
{
    size_t exec1{ 0 };
    WorkerHandle count = std::make_shared<Countdown>(3, &exec1);
    queue.put(count);

    size_t exec2{ 0 };
    WorkerHandle start = std::make_shared<OneShotWorker>(&exec2);
    queue.put(start);

    Trampoline(queue);

    EXPECT_EQ(4, exec1);
    EXPECT_EQ(1, exec2);
}
