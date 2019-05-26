#if ! defined(EXTENDEDNUMERIC_HPP)
#define EXTENDEDNUMERIC_HPP 1

#include "ScamFwd.hpp"
#include "expr/ValueWriter.hpp"

namespace scam
{
    class ExtendedNumeric
    {
    public:
        explicit ExtendedNumeric(ConstScamValue expr);

        ScamValue get() const;
	
    private:
        ConstScamValue expr;
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
        const char * value =  expr ? writeValue(expr).c_str() : "<null>";
        os << expr << "\t" << value;
        return os;
    }

}

#endif
