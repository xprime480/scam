#if ! defined(SCAMCLOSURE_H)
#define SCAMCLOSURE_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class Env;
    class LambdaParser;

    class ScamClosure : public ScamExpr
    {
    private:
        friend class MemoryManager;

        ScamClosure(const LambdaParser * parser,
                    Env * env,
                    bool macrolike = false);

        static ScamClosure * makeInstance(const LambdaParser * parser,
                                          Env * env,
                                          bool macrolike = false);

    public:
        void mark() const override;

        std::string toString() const override;

        void
        apply(ExprHandle args, Continuation * cont, Env * env) override;

        ExprHandle withEnvUpdate(Env * updated) const override;

    private:
        const LambdaParser * parser;
        Env * env;
        bool macrolike;
    };
}

#endif
