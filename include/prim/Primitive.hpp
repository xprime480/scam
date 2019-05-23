#if ! defined(PRIMITIVE_H)
#define PRIMITIVE_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class Primitive : public ScamExpr
    {
    public:
        Primitive(std::string const & name);

        void apply(ScamValue args, Continuation * cont, Env * env) override;

        virtual void
        applyArgs(ScamValue args, Continuation * cont) = 0;
    };
}

#endif
