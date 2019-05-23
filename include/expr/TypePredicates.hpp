#if ! defined(TYPEPREDICATES_HPP)
#define TYPEPREDICATES_HPP 1

namespace scam
{
    class ScamData;

    class TypePredicates
    {
    public:
        static bool isNull(const ScamData * data);
        static bool error(const ScamData * data);
        static bool truth(const ScamData * data);

        static bool isBoolean(const ScamData * data);
        static bool isChar(const ScamData * data);
        static bool isString(const ScamData * data);
        static bool isSymbol(const ScamData * data);
        static bool isKeyword(const ScamData * data);

        static bool isNumeric(const ScamData * data);
        static bool isExact(const ScamData * data);
        static bool isComplex(const ScamData * data);
        static bool isPureComplex(const ScamData * data);
        static bool isReal(const ScamData * data);
        static bool isRational(const ScamData * data);
        static bool isInteger(const ScamData * data);

        static bool isNaN(const ScamData * data);
        static bool isNegInf(const ScamData * data);
        static bool isPosInf(const ScamData * data);

        static bool isNil(const ScamData * data);
        static bool isCons(const ScamData * data);
        static bool isList(const ScamData * data);

        static bool isVector(const ScamData * data);
        static bool isByteVector(const ScamData * data);
        static bool isDict(const ScamData * data);

        static bool isProcedure(const ScamData * data);
        static bool isClass(const ScamData * data);
        static bool isInstance(const ScamData * data);
    };
}

#endif
