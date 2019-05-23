#if ! defined(ORCONT_HPP)
#define ORCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ListParser;

    class OrCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        OrCont(ListParser * parser, Continuation * cont, Env * env, size_t n);

        static OrCont * makeInstance(ListParser * parser,
                                     Continuation * cont,
                                     Env * env,
                                     size_t n);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        ListParser * parser;
        Continuation * cont;
        Env * env;
        size_t n;
    };
}

#endif
