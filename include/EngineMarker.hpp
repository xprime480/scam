#if ! defined(ENGINEMARKER_HPP)
#define ENGINEMARKER_HPP 1

#include "util/MemoryManager.hpp"

namespace scam
{
    class ScamEngine;

    class EngineMarker : public MemoryManager::Hook
    {
    public:
        void markRoots() const override;
        void releaseRoots() override;
    };
}

#endif
