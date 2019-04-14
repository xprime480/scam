#include "EngineMarker.hpp"

#include "ScamEngine.hpp"

using namespace scam;

EngineMarker::EngineMarker(ScamEngine * engine)
    : engine(engine)
{
}

void EngineMarker::operator()() const
{
    engine->mark();
}
