#if ! defined(LETCOMMONCONT_HPP)
#define LETCOMMONCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetCommonCont : public Continuation
    {
    protected:
        LetCommonCont(char const * name, ScamValue forms, Continuation * cont);

    public:
        void mark() const override;

        void run(ScamValue expr) override;

    protected:
        ScamValue forms;
        Continuation * cont;

        virtual void do_let(ScamValue expr) = 0;
        void final_eval(Env * env);
    };
}

#endif
