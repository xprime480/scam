
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

int main(int argc, char ** argv)
{
    WorkQueue queue;
    WorkerHandle start = std::make_shared<OneShotWorker>();
    queue.put(start);
    Trampoline(queue);
}
