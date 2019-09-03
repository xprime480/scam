#include "EngineMarker.hpp"

#include "ScamEngine.hpp"

using namespace scam;

void EngineMarker::operator()() const
{
    ScamEngine::getEngine().mark();
}
