#if ! defined(LETBASEWORKER_HPP)
#define LETBASEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"
#include "util/LetDef.hpp"

namespace scam
{

    class LetBaseWorker : public Worker
    {
    protected:
        LetBaseWorker(char const * name,
                      LetDef & def,
                      Continuation * cont,
                      Env * env);

    public:
        void mark() override;
        void run() override;

    protected:
        Continuation * cont;
        Env * env;

        virtual void
        do_next(ScamValue formals, ScamValue values, ScamValue forms) = 0;

    private:
        LetDef def;
    };
}

#endif

