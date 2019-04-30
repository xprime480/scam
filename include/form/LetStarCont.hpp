#if ! defined(LETSTARCONT_HPP)
#define LETSTARCONT_HPP 1

#include "LetCommonCont.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetStarCont : public LetCommonCont
    {
    private:
        friend class scam::MemoryManager;

        LetStarCont(ExprHandle formals,
                    ExprHandle rest,
                    ExprHandle forms,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine);

        static LetStarCont * makeInstance(ExprHandle formals,
                                          ExprHandle rest,
                                          ExprHandle forms,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine);

    public:
        void mark() const override;

    protected:
        void do_let(ExprHandle expr) override;

    private:
        ExprHandle formals;
        ExprHandle rest;
        Env *        env;
        ScamEngine * engine;

        void makeBacktracker(ExprHandle sym) const;
    };
}

#endif
