#if ! defined(SCAMFWD_HPP)
#define SCAMFWD_HPP 1

namespace scam
{
    class Continuation;
    class Env;
    class MemoryManager;
    class ScamEngine;
    class ScamExpr;

    /* at some point in the future we will deal with constness */

    using ExprHandle = /*const*/ ScamExpr *;

    /* for those cases we know it must be const */

    using ConstExprHandle = const ScamExpr *;

    /* for Env related functions */

    class ScamSymbol;
    using ScamEnvKeyType = const ScamSymbol *;
}

#endif
