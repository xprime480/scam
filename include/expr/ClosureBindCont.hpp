#if ! defined(CLOSUREBINDCONT_HPP)
#define CLOSUREBINDCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class Env;
    class ScamExpr;
    class MemoryManager;

    class ClosureBindCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ClosureBindCont(ScamExpr * formals,
                        ScamExpr * forms,
                        Env * capture,
                        Continuation * cont,
                        bool macrolike);

        static ClosureBindCont * makeInstance(ScamExpr * formals,
                                              ScamExpr * forms,
                                              Env * capture,
                                              Continuation * cont,
                                              bool macrolike);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        ScamExpr * formals;
        ScamExpr * forms;
        Env *        capture;
        Continuation * cont;
        bool       macrolike;

        bool malformedActuals(ScamExpr * expr) const;
        bool describeFormals(unsigned & len) const;

        void wrongNumberOfParameters(unsigned formalsLen,
                                     unsigned actualsLen) const;

        bool checkArgLength(ScamExpr * expr) const;
        void finalize(ScamExpr * actuals)  const;
    };
}

#endif
