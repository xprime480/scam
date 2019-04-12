#if ! defined(ENVHELPERWORKER_HPP)
#define ENVHELPERWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;

    class EnvHelperWorker : public Worker
    {
    protected:
        EnvHelperWorker(ScamExpr * args,
                        Continuation * cont,
                        Env * env,
                        char const * name);

    public:
        void mark() const override;
        void run() override;

    protected:
        Continuation * cont;
        Env * env;

        virtual Continuation * getCont(ScamExpr * sym) const = 0;

    private:
        ScamExpr * args;
    };
}

#endif
