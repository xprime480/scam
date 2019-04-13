
#include "util/DebugTrace.hpp"

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

