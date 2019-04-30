#include "OneShotWorker.hpp"
#include "CountdownWorker.hpp"

#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "util/MemoryManager.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;
using namespace scam::test_impl;

namespace
{
    WorkQueue queue;
}

TEST(TrampolineTest, SimpleTest)
{
    MemoryManager & mm = scam::standardMemoryManager;

    size_t exec1{ 0 };
    Worker * count = mm.make<CountdownWorker>(3, &exec1, queue);
    queue.put(count);

    size_t exec2{ 0 };
    Worker * start = mm.make<OneShotWorker>(&exec2);
    queue.put(start);

    Trampoline(queue);

    EXPECT_EQ(4, exec1);
    EXPECT_EQ(1, exec2);
}
