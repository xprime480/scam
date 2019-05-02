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
        do_next(ExprHandle formals, ExprHandle values, ExprHandle forms) = 0;

    private:
        LetParser * parser;

        ExprHandle parse_bindings();
        ExprHandle parse_args();
    };
}

#endif

