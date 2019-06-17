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

        ClosureBindCont(LambdaParser * lambda,
                        Env * capture,
                        Continuation * cont,
                        bool macrolike,
                        ScamEngine * engine);

        static ClosureBindCont * makeInstance(LambdaParser * lambda,
                                              Env * capture,
                                              Continuation * cont,
                                              bool macrolike,
                                              ScamEngine * engine);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        LambdaParser * lambda;
        Env          * capture;
        Continuation * cont;
        bool           macrolike;

        bool malformedActuals(ScamValue expr) const;
        bool describeFormals(unsigned & len) const;

        void wrongNumberOfParameters(unsigned formalsLen,
                                     unsigned actualsLen) const;

        bool checkArgLength(ScamValue expr) const;
        void finalize(ScamValue actuals)  const;
    };
}

#endif
