#if ! defined(ENVHELPERWORKER_HPP)
#define ENVHELPERWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    
    class Continuation;
    class Env;

    class EnvHelperWorker : public Worker
    {
    protected:
        EnvHelperWorker(ExprHandle args,
                        Continuation * cont,
                        Env * env,
                        char const * name);

    public:
        void mark() const override;
        void run() override;

    protected:
        Continuation * cont;
        Env * env;

        virtual Continuation * getCont(ExprHandle sym) const = 0;

    private:
        ExprHandle args;
    };
}

#endif
