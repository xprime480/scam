#if ! defined(SCAMCLASSADAPTER_H)
#define SCAMCLASSADAPTER_H 1

#include "Env.hpp"

namespace scam
{
    class ScamExpr;
    class ScamClass;

    class ScamClassAdapter
    {
    public:
        ScamClassAdapter(ScamExpr const * expr);

        ScamExpr * getBase() const;
        ScamExpr * getVars() const;
        ScamExpr * getFuns() const;
        Env getCapture() const;

    private:
        ScamClass const * cls;
    };
}

#endif
