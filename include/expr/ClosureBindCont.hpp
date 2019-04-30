#if ! defined(CLOSUREBINDCONT_HPP)
#define CLOSUREBINDCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class Env;
    class LambdaParser;

    class MemoryManager;

    class ClosureBindCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ClosureBindCont(const LambdaParser * lambda,
                        Env * capture,
                        Continuation * cont,
                        bool macrolike);

        static ClosureBindCont * makeInstance(const LambdaParser * lambda,
                                              Env * capture,
                                              Continuation * cont,
                                              bool macrolike);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        const LambdaParser * lambda;
        Env *        capture;
        Continuation * cont;
        bool       macrolike;

        bool malformedActuals(ExprHandle expr) const;
        bool describeFormals(unsigned & len) const;

        void wrongNumberOfParameters(unsigned formalsLen,
                                     unsigned actualsLen) const;

        bool checkArgLength(ExprHandle expr) const;
        void finalize(ExprHandle actuals)  const;
    };
}

#endif
