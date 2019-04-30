#if ! defined(VECTORCONT_HPP)
#define VECTORCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"
#include "expr/ScamVector.hpp"

namespace scam
{
    class VectorCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        VectorCont(ExprVec const & forms,
                   ExprVec const & evaled,
                   Continuation * original,
                   Env * env);

        static VectorCont * makeInstance(ExprVec const & forms,
                                         ExprVec const & evaled,
                                         Continuation * original,
                                         Env * env);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        ExprVec forms;
        ExprVec evaled;
        Continuation * original;
        Env * env;
    };
}

#endif
