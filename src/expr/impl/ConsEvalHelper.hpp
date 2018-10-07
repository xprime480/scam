#if ! defined(CONS_EVAL_HELPER_HPP)
# define CONS_EVAL_HELPER_HPP 1

#include "Worker.hpp"

#include <memory>

namespace scam
{
    class Continuation;
    class Env;
    class ScamExpr;

    using ContHandle = std::shared_ptr<Continuation> ;
    using ExprHandle = std::shared_ptr<ScamExpr>;

    namespace cons_impl
    {
        struct WorkerData;

        class  ConsWorker : public Worker
        {
        public:
            ConsWorker(ExprHandle & car,
                       ExprHandle & cdr,
                       ContHandle cont,
                       Env & env);

            ConsWorker(std::shared_ptr<WorkerData> data);

            void run() override;

        private:
            std::shared_ptr<WorkerData> data;
        };
    }
}

#endif

