#if ! defined(EXTENDEDNUMERIC_HPP)
#define EXTENDEDNUMERIC_HPP 1

#include "ScamFwd.hpp"
#include "expr/ExprWriter.hpp"
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
        const ScamData * expr = num.get();
        const char * value =  expr ? ExprWriter::write(expr).c_str() : "<null>";
        os << expr << "\t" << value;
        return os;
    }

}

#endif
