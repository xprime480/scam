#if ! defined(CONS_HELPER_HPP)
# define CONS_HELPER_HPP 1

#include "Env.hpp"
#include "Continuation.hpp"

#include "expr/ScamExpr.hpp"

namespace scam
{
    namespace cons_impl
    {
        void scamConsEvalHelper(ScamExpr * car,
                                ScamExpr * cdr,
                                Continuation * cont,
                                Env * env);

        void scamConsMapHelper(ScamExpr * car,
                               ScamExpr * cdr,
                               Continuation * cont,
                               Env * env);

        struct WorkerData
        {
            WorkerData(ScamExpr * car,
                       ScamExpr * cdr,
                       Continuation * original,
                       Env * env)
                : car(car)
                , cdr(cdr)
                , original(original)
                , env(env)
            {
            }

            WorkerData(const WorkerData &) = default;
            WorkerData & operator=(const WorkerData &) = default;

            void mark() const
            {
               car->mark();
               cdr->mark();
               original->mark();
               cont->mark();
	       env->mark();
            }

            ScamExpr * car;
            ScamExpr * cdr;
            Continuation * original;
            Continuation * cont;
            Env * env;
        };
    }
}

#endif

