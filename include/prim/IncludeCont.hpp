#if ! defined(INCLUDECONT_HPP)
#define INCLUDECONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class IncludeParser * parser;

    class IncludeCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        IncludeCont(IncludeParser * parser,
                    Continuation * cont,
                    ScamEngine * engine,
                    size_t nextIdx);

        static IncludeCont * makeInstance(IncludeParser * parser,
                                          Continuation * cont,
                                          ScamEngine * engine,
                                          size_t nextIdx);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        IncludeParser * parser;
        Continuation * cont;
        size_t nextIdx;
    };
}

#endif
