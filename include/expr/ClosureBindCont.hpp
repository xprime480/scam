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
                        bool macrolike,
                        ScamEngine * engine);

        static ClosureBindCont * makeInstance(const LambdaParser * lambda,
                                              Env * capture,
                                              Continuation * cont,
                                              bool macrolike,
                                              ScamEngine * engine);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        const LambdaParser * lambda;
        Env *        capture;
        Continuation * cont;
        bool       macrolike;

        bool malformedActuals(ScamValue expr) const;
        bool describeFormals(unsigned & len) const;

        void wrongNumberOfParameters(unsigned formalsLen,
                                     unsigned actualsLen) const;

        bool checkArgLength(ScamValue expr) const;
        void finalize(ScamValue actuals)  const;
    };
}

#endif
