#if ! defined(CLOSUREBINDCONT_HPP)
#define CLOSUREBINDCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class Env;
    class LambdaDef;

    class MemoryManager;

    class ClosureBindCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ClosureBindCont(LambdaDef & lambda,
                        Env * capture,
                        Continuation * cont,
                        ScamEngine * engine);

        static ClosureBindCont * makeInstance(LambdaDef & lambda,
                                              Env * capture,
                                              Continuation * cont,
                                              ScamEngine * engine);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        LambdaDef    & lambda;
        Env          * capture;
        Continuation * cont;

        bool malformedActuals(ScamValue expr) const;
        bool describeFormals(unsigned & len) const;

        void wrongNumberOfParameters(unsigned formalsLen,
                                     unsigned actualsLen) const;

        bool checkArgLength(ScamValue expr) const;
        ScamValue finalize(ScamValue actuals)  const;
    };
}

#endif
