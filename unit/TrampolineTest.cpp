
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

#include <iostream>

using namespace std;
using namespace scam;

namespace
{
    WorkQueue queue;
}

class TestWorker : public Worker
{
public:
    TestWorker(size_t * counter)
	: counter(counter)
    {
    }

    void run() override
    {
	++*counter;
    }
    
protected:
    size_t * counter;
};


class OneShotWorker : public TestWorker
{
public:
    OneShotWorker(size_t * counter)
	: TestWorker(counter)
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
	: TestWorker(counter)
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

bool trampolinetest()
{
    size_t exec1{ 0 };
    WorkerHandle count = std::make_shared<Countdown>(3, &exec1);
    queue.put(count);

    size_t exec2{ 0 };
    WorkerHandle start = std::make_shared<OneShotWorker>(&exec2);
    queue.put(start);

    Trampoline(queue);

    bool ok { true };
    if ( 4 != exec1 ) {
	cerr << "Expected 4 countdown reps, got " << exec1 << "\n";
	ok = false;
    }
    if ( 1 != exec2 ) {
	cerr << "Expected 1 one-shot rep, got " << exec2 << "\n";
	ok = false;
    }

    return ok;
}
