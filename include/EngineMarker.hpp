#if ! defined(ENGINEMARKER_HPP)
#define ENGINEMARKER_HPP 1

#include "util/MemoryManager.hpp"

namespace scam
{
    class ScamEngine;

    class EngineMarker : public MemoryManager::Hook
    {
    public:
        EngineMarker(ScamEngine * engine);
        void operator()() const override;

    private:
        ScamEngine * engine;
    };
}

#endif
