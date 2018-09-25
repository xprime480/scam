#include "ScamEngine.hpp"

#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

#include "output/OutputHandler.hpp"

using namespace scam;

WorkQueue globalWorkQueue;


ScamEngine::ScamEngine()
{
}

void ScamEngine::repl(Tokenizer & input, OutputHandler & output)
{
    Trampoline(globalWorkQueue);
}

void ScamEngine::extend(std::string const & name,
                          Tokenizer & input,
                          OutputHandler & output)
{
}
