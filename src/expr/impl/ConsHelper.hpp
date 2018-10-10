#if ! defined(CONS_HELPER_HPP)
# define CONS_HELPER_HPP 1

#include "WorkQueue.hpp"

#include <memory>

namespace scam
{
    namespace cons_impl
    {
        void scamConsEvalHelper(ExprHandle car,
                                ExprHandle cdr,
                                ContHandle cont,
                                Env & env);

        void scamConsMapHelper(ExprHandle & car,
                               ExprHandle & cdr,
                               ContHandle cont,
                               Env & env);

        struct WorkerData
        {
            WorkerData(ExprHandle car,
                       ExprHandle cdr,
                       ContHandle original,
                       Env & env)
                : car(car)
                , cdr(cdr)
                , original(original)
                , env(env)
            {
            }

            ExprHandle car;
            ExprHandle cdr;
            ContHandle original;
            ContHandle cont;
            Env & env;
        };
    }
}

#endif

