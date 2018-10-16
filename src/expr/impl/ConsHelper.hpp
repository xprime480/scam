#if ! defined(CONS_HELPER_HPP)
# define CONS_HELPER_HPP 1

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "WorkQueue.hpp"

#include <memory>

namespace scam
{
    namespace cons_impl
    {
        void scamConsEvalHelper(ScamExpr * car,
                                ScamExpr * cdr,
                                ContHandle cont,
                                Env env);

        void scamConsMapHelper(ScamExpr * car,
                               ScamExpr * cdr,
                               ContHandle cont,
                               Env env);

        struct WorkerData
        {
            WorkerData(ScamExpr * car,
                       ScamExpr * cdr,
                       ContHandle original,
                       Env env)
                : car(car->clone())
                , cdr(cdr->clone())
                , original(original)
                , env(env)
            {
            }

            WorkerData(const WorkerData &) = default;
            WorkerData & operator=(const WorkerData &) = default;

            ExprHandle car;
            ExprHandle cdr;
            ContHandle original;
            ContHandle cont;
            Env env;
        };
    }
}

#endif

