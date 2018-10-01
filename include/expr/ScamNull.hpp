#if ! defined(SCAMNULL_H)
#define SCAMNULL_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class ScamContext;

    class ScamNull : public ScamExpr
    {
    public:
        virtual std::string toString() const;
        virtual void eval(ScamContext & context);

        virtual bool isNull() const;
        virtual bool truth() const;
    };
}

#endif
