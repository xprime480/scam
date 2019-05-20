#if ! defined(EXTENDEDNUMERIC_HPP)
#define EXTENDEDNUMERIC_HPP 1

#include "ScamFwd.hpp"
#include "expr/ScamExpr.hpp"

namespace scam
{
    class ExtendedNumeric
    {
    public:
        explicit ExtendedNumeric(ConstExprHandle expr);

        ExprHandle get() const;

        bool isNaN() const;
        bool isNegInf() const;
        bool isPosInf() const;

        bool isSpecialNumeric() const;

    private:
        ConstExprHandle expr;
    };

    /* relational operators on extended numerics */

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

    /* arithemetic operators on extended numerics */

    extern ExtendedNumeric
    operator+(const ExtendedNumeric & a, const ExtendedNumeric & b);

    extern ExtendedNumeric
    operator-(const ExtendedNumeric & a);

    extern ExtendedNumeric
    operator-(const ExtendedNumeric & a, const ExtendedNumeric & b);

    extern ExtendedNumeric
    operator*(const ExtendedNumeric & a, const ExtendedNumeric & b);

    extern ExtendedNumeric
    operator/(const ExtendedNumeric & a, const ExtendedNumeric & b);

    extern ExtendedNumeric
    operator%(const ExtendedNumeric & a, const ExtendedNumeric & b);

    /* print a representation of an extended numeric */

    template <typename OS>
    OS & operator<<(OS & os, ExtendedNumeric num)
    {
        ExprHandle expr = num.get();
        os << expr << "\t" << (expr ? expr->toString() : "<null>");
        return os;
    }

}

#endif
