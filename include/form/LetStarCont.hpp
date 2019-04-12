#if ! defined(LETSTARCONT_HPP)
#define LETSTARCONT_HPP 1

#include "LetCommonCont.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;
    class ScamEngine;

    class LetStarCont : public LetCommonCont
    {
    private:
        friend class scam::MemoryManager;

        LetStarCont(ScamExpr * formals,
                    ScamExpr * rest,
                    ScamExpr * forms,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine);

        static LetStarCont * makeInstance(ScamExpr * formals,
                                          ScamExpr * rest,
                                          ScamExpr * forms,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine);

    public:
        void mark() const override;

    protected:
        void do_let(ScamExpr * expr) override;

    private:
        ScamExpr * formals;
        ScamExpr * rest;
        Env *        env;
        ScamEngine * engine;

        void makeBacktracker(ScamExpr * sym) const;
    };
}

#endif
