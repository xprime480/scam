#if ! defined(ANDCONT_HPP)
#define ANDCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ListParser;

    class AndCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        AndCont(ListParser * parser,
                Continuation * cont,
                Env * env,
                ScamEngine * engine,
                size_t n);

        static AndCont * makeInstance(ListParser * parser,
                                      Continuation * cont,
                                      Env * env,
                                      ScamEngine * engine,
                                      size_t n);

    public:
        void mark() const override;

        void handleValue(ScamValue expr) override;

    private:
        ListParser * parser;
        Continuation * cont;
        Env * env;
        size_t n;
    };
}

#endif
