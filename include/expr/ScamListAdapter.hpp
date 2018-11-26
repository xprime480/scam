#if ! defined(SCAMLISTADAPTER_H)
#define SCAMLISTADAPTER_H 1

#include "Env.hpp"
#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCons;

    class ScamListAdapter
    {
    public:
        ScamListAdapter(ScamExpr const * expr);

        size_t getLength() const;
        ExprHandle append(ScamExpr * tail) const;

    private:
        ScamCons const * lst;
        bool isNil;

        size_t len;
	ExprHandle car;
	ExprHandle cdr;
    };
}

#endif
