#if ! defined(SCAMCLOSURE_H)
#define SCAMCLOSURE_H 1

#include "expr/ScamData.hpp"

namespace scam
{
    class Env;
    class LambdaParser;

    class ScamClosure : public ScamData
    {
    private:
        friend class MemoryManager;

        ScamClosure(const LambdaParser * parser,
                    Env * env,
                    bool macrolike = false);

        static ScamClosure * makeInstance(const LambdaParser * parser,
                                          Env * env,
                                          bool macrolike = false);
    };
}

#endif
