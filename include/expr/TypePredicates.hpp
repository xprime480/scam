#if ! defined(TYPEPREDICATES_HPP)
#define TYPEPREDICATES_HPP 1

namespace scam
{
    class ScamData;

    extern bool isNull(const ScamData * data);
    extern bool error(const ScamData * data);
    extern bool truth(const ScamData * data);

    extern bool isBoolean(const ScamData * data);
    extern bool isChar(const ScamData * data);
    extern bool isString(const ScamData * data);
    extern bool isSymbol(const ScamData * data);
    extern bool isKeyword(const ScamData * data);

    extern bool isNumeric(const ScamData * data);
    extern bool isExact(const ScamData * data);
    extern bool isComplex(const ScamData * data);
    extern bool isPureComplex(const ScamData * data);
    extern bool isReal(const ScamData * data);
    extern bool isRational(const ScamData * data);
    extern bool isInteger(const ScamData * data);

    extern bool isNaN(const ScamData * data);
    extern bool isNegInf(const ScamData * data);
    extern bool isPosInf(const ScamData * data);
    extern bool isSpecialNumeric(const ScamData * data);

    extern bool isNil(const ScamData * data);
    extern bool isCons(const ScamData * data);
    extern bool isList(const ScamData * data);

    extern bool isVector(const ScamData * data);
    extern bool isByteVector(const ScamData * data);
    extern bool isDict(const ScamData * data);

    extern bool isProcedure(const ScamData * data);
    extern bool isClass(const ScamData * data);
    extern bool isInstance(const ScamData * data);
}

#endif
