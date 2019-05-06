#if ! defined(INCLUDEWORKER_HPP)
#define INCLUDEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class IncludeParser;

    class IncludeWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        IncludeWorker(IncludeParser * parser,
                      Continuation * cont,
                      ScamEngine * engine,
                      size_t idx);

        static IncludeWorker * makeInstance(IncludeParser * parser,
                                            Continuation * cont,
                                            ScamEngine * engine,
                                            size_t idx);

    public:
        void mark() const override;
        void run() override;

    private:
        IncludeParser * parser;
        Continuation * cont;
        ScamEngine * engine;
        const size_t  idx;;
    };
}

#endif
