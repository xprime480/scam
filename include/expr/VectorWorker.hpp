#if ! defined(VECTORWORKER_HPP)
#define VECTORWORKER_HPP 1

#include "Worker.hpp"
#include "expr/ScamVector.hpp"

namespace scam
{
    class Continuation;
    class MemoryManager;
    class Env;

    class VectorWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        VectorWorker(Continuation * cont,
                     Env * env,
                     ExprVec const & forms);

        VectorWorker(Continuation * cont,
                     Env * env,
                     ExprVec const & forms,
                     ExprVec const & evaled);

        static VectorWorker * makeInstance(Continuation * cont,
                                           Env * env,
                                           ExprVec const & forms);

        static VectorWorker * makeInstance(Continuation * cont,
                                           Env * env,
                                           ExprVec const & forms,
                                           ExprVec const & evaled);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprVec    forms;
        ExprVec    evaled;
        Continuation * original;
        Env *        env;
    };
}

#endif
