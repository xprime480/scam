#if ! defined(TYPEPREDICATES_HPP)
#define TYPEPREDICATES_HPP 1

namespace scam
{
    class ScamData;

    class TypePredicates
    {
    public:
        static bool isNull(const ScamData *);
        static bool error(const ScamData *);
        static bool truth(const ScamData *);

        static bool isBoolean(const ScamData *);
        static bool isChar(const ScamData *);
        static bool isString(const ScamData *);
        static bool isSymbol(const ScamData *);
        static bool isKeyword(const ScamData *);

        static bool isNumeric(const ScamData *);
        static bool isExact(const ScamData *);
        static bool isComplex(const ScamData *);
        static bool isReal(const ScamData *);
        static bool isRational(const ScamData *);
        static bool isInteger(const ScamData *);

        static bool isNaN(const ScamData *);
        static bool isNegInf(const ScamData *);
        static bool isPosInf(const ScamData *);

        static bool isNil(const ScamData *);
        static bool isCons(const ScamData *);
        static bool isList(const ScamData *);

        static bool isVector(const ScamData *);
        static bool isByteVector(const ScamData *);
        static bool isDict(const ScamData *);

        static bool isProcedure(const ScamData *);
        static bool isClass(const ScamData *);
        static bool isInstance(const ScamData *);
    };
}

#endif
