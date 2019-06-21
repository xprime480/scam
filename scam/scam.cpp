#include "ScamRepl.hpp"

using namespace scam;

int main(int argc, char ** argv)
{
    ScamRepl repl(argc, argv);
    return repl.run();
}
