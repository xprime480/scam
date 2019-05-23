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
        ScamListAdapter(ConstScamValue expr);

        size_t getLength() const;
        ScamValue append(ScamValue tail) const;

    private:
        ScamCons const * lst;
        bool isNil;

        size_t len;
        ScamValue car;
        ScamValue cdr;
    };
}

#endif
