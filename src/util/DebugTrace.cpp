#include "util/DebugTrace.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "port/ScamPort.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

bool scam::scamIsTracing = false;

static size_t counter { 0 };

scam::ScamTraceScope::ScamTraceScope()
{
    ++counter;
    scam::scamIsTracing = true;
}

scam::ScamTraceScope::~ScamTraceScope()
{
    --counter;
    if ( 0 == counter ) {
      scam::scamIsTracing = false;
    }
}

void scam::scamLog(const std::string & msg)
{
    ScamPort * port = nullptr;
    Env * configEnv = ScamEngine::getEngine().getConfigFrame();

    if ( configEnv ) {
        const ScamValue portName = makeSymbol("**log-port**");
        if ( truth(configEnv->check(portName)) ) {
            ScamValue p = configEnv->get(portName);
            if ( isPort(p) ) {
                port = asPort(p);
                if ( port->isWriteable() ) {
                    port->put(msg.c_str(), msg.size());
                }
                else {
                    port = nullptr;
                }
            }
        }
    }

    if ( ! port ) {
        cerr << msg;
    }
}
