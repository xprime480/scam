#if ! defined(EXTENDEDNUMERIC_HPP)
#define EXTENDEDNUMERIC_HPP 1

#include "ScamFwd.hpp"
#include "expr/ScamExpr.hpp"

namespace scam
{
    class ExtendedNumeric
    {
    public:
        explicit ExtendedNumeric(ExprHandle expr);

        ExprHandle get() const;

        bool isNaN() const;
        bool isNegInf() const;
        bool isPosInf() const;

    private:
        ExprHandle expr;
    };

    extern bool
    operator==(const ExtendedNumeric & a, const ExtendedNumeric & b);

    extern bool
    operator!=(const ExtendedNumeric & a, const ExtendedNumeric & b);

    extern bool
    operator>(const ExtendedNumeric & a, const ExtendedNumeric & b);

    extern bool
    operator>=(const ExtendedNumeric & a, const ExtendedNumeric & b);

    extern bool
    operator<(const ExtendedNumeric & a, const ExtendedNumeric & b);

    extern bool
    operator<=(const ExtendedNumeric & a, const ExtendedNumeric & b);

    template <typename OS>
    OS & operator<<(OS & os, ExtendedNumeric num)
    {
        ExprHandle expr = num.get();
        os << expr << "\t" << (expr ? expr->toString() : "<null>");
        return os;
    }

}

#endif
