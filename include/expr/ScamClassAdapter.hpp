#if ! defined(SCAMCLASSADAPTER_H)
#define SCAMCLASSADAPTER_H 1

#include "ScamFwd.hpp"

namespace scam
{
    class ScamClass;

    class ScamClassAdapter
    {
    public:
        ScamClassAdapter(ConstExprHandle expr);

        ExprHandle getBase() const;
        ExprHandle getVars() const;
        ExprHandle getFuns() const;
        Env * getCapture() const;

    private:
        ScamClass const * cls;
    };
}

#endif
