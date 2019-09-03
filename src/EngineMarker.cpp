#include "EngineMarker.hpp"

#include "ScamEngine.hpp"

using namespace scam;

void EngineMarker::markRoots() const
{
    ScamEngine::getEngine().mark();
}

void EngineMarker::releaseRoots()
{
    ScamEngine::getEngine().release();
}
