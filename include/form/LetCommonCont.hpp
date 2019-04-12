#if ! defined(LETCOMMONCONT_HPP)
#define LETCOMMONCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class Continuation;
    class ScamExpr;
    class Env;

    class LetCommonCont : public Continuation
    {
    protected:
        LetCommonCont(char const * name, ScamExpr * forms, Continuation * cont);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    protected:
        ScamExpr * forms;
        Continuation * cont;

        virtual void do_let(ScamExpr * expr) = 0;
        void final_eval(Env * env);
    };
}

#endif
