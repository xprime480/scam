#if ! defined(SCAMEXPR_HPP)
#define SCAMEXPR_HPP 1

#include "expr/ScamData.hpp"

#include "ScamFwd.hpp"

#include <string>

namespace scam
{
    class ScamExpr : public ScamData
    {
    protected:
        ScamExpr(unsigned long type, bool managed = true);
    };
}

#endif
