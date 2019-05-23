#if ! defined(LETBASEWORKER_HPP)
#define LETBASEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetParser;

    class LetBaseWorker : public Worker
    {
    protected:
        LetBaseWorker(char const * name,
                      LetParser * parser,
                      Continuation * cont,
                      Env * env);

    public:
        void mark() const override;
        void run() override;

    protected:
        Continuation * cont;
        Env * env;

        virtual void
        do_next(ScamValue formals, ScamValue values, ScamValue forms) = 0;

    private:
        LetParser * parser;

        ScamValue parse_bindings();
        ScamValue parse_args();
    };
}

#endif

