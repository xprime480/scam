
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

#include <iostream>

using namespace std;
using namespace scam;

class OneShotWorker : public Worker
{
public:
    void run() override
    {
        cout << "OneShotWorker here\n";
    }
};

class Countdown : public Worker
{
public:
    Countdown(size_t n, WorkQueue & queue)
        : n(n)
        , queue(queue)
    {
    }

    void run() override
    {
        if ( 0 == n ) {
            cout << "go!!\n";
        }
        else {
            cout << n << "... ";

            WorkerHandle next = std::make_shared<Countdown>(n-1, queue);
            queue.put(next);
        }
    }

private:
    size_t n;
    WorkQueue & queue;
};

int main(int argc, char ** argv)
{
    WorkQueue queue;

    WorkerHandle count = std::make_shared<Countdown>(3, queue);
    queue.put(count);

    WorkerHandle start = std::make_shared<OneShotWorker>();
    queue.put(start);

    Trampoline(queue);
}
