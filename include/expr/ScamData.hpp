#if ! defined(SCAMDATA_HPP)
#define SCAMDATA_HPP 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"

#include <functional>
#include <string>
#include <vector>

namespace scam
{
    class ClassDefParser;
    class Continuation;
    class Env;
    class LambdaParser;
    class ScamPort;

    using SfFunction =
        std::function<void(ScamValue,
                           Continuation *,
                           Env *,
                           ScamEngine *)>;
    using PrimFunction =
        std::function<void(ScamValue,
                           Continuation *,
                           ScamEngine *)>;

    struct ScamData : public ManagedObject
    {
    public:
        using ValueType = unsigned long;

        explicit ScamData(ValueType type, bool managed = true);
        ~ScamData();

        ScamData(const ScamData &) = delete;
        ScamData operator=(const ScamData &) = delete;
        ScamData(ScamData &&) = delete;
        ScamData operator=(ScamData &&) = delete;

        static ScamData * makeInstance(ValueType type, bool managed = true);

        void mark() override final;

        void makeImmutable();
        bool isImmutable() const;

        void setMeta(std::string const & key, ScamValue value) const;
        bool hasMeta(std::string const & key) const;
        ScamValue getMeta(std::string const & key) const;

        /*
         * tags and types for numeric types
         */
        class NaNType {};
        class NegInfType {};
        class PosInfType {};

        constexpr static ValueType Complex        { 1 << 0 };
        constexpr static ValueType Real           { 1 << 1 };
        constexpr static ValueType Rational       { 1 << 2 };
        constexpr static ValueType Integer        { 1 << 3 };
        constexpr static ValueType NaN            { 1 << 4 };
        constexpr static ValueType NegInf         { 1 << 5 };
        constexpr static ValueType PosInf         { 1 << 6 };
        constexpr static ValueType SpecialNumeric { NaN | NegInf | PosInf };
        constexpr static ValueType RationalTypes  { Rational | Integer };
        constexpr static ValueType RealNumTypes   { Real | RationalTypes };
        constexpr static ValueType RealTypes      { RealNumTypes | SpecialNumeric };
        constexpr static ValueType ComplexTypes   { Complex | RealTypes };
        constexpr static ValueType Numeric        { ComplexTypes | RealTypes };

        /*
         * tags and types for other atoms
         */
        constexpr static ValueType Nothing      { 1 << 7 };
        constexpr static ValueType Null         { 1 << 8 };
        constexpr static ValueType Boolean      { 1 << 9 };
        constexpr static ValueType Character    { 1 << 10 };
        constexpr static ValueType Symbol       { 1 << 11 };
        constexpr static ValueType Keyword      { 1 << 12 };
        constexpr static ValueType String       { 1 << 13 };
        constexpr static ValueType Error        { 1 << 14 };
        constexpr static ValueType Pair         { 1 << 15 };
        constexpr static ValueType Vector       { 1 << 16 };
        constexpr static ValueType ByteVector   { 1 << 17 };
        constexpr static ValueType Dict         { 1 << 18 };
        constexpr static ValueType Closure      { 1 << 19 };
        constexpr static ValueType Class        { 1 << 20 };
        constexpr static ValueType Instance     { 1 << 21 };
        constexpr static ValueType Cont         { 1 << 22 };

        constexpr static ValueType StringLike   { Symbol | Keyword | String };

        constexpr static ValueType Procedure    { Closure | Class | Instance };

        constexpr static ValueType Primitive   { 1 << 23 };
        constexpr static ValueType SpecialForm { 1 << 24 };

        constexpr static ValueType Applicable  { Dict | Procedure | Primitive | SpecialForm | Cont };

        constexpr static ValueType Port    { 1 << 25 };
        constexpr static ValueType Eof     { 1 << 26 };

        /**
         * member data
         */
        const ValueType type;

    private:
        bool immutable;
    public:
        struct ComplexData
        {
            ScamValue real;
            ScamValue imag;
        };

        struct RationalData
        {
            int num;
            int den;
        };

        struct NumericData
        {
            bool exact;
            union
            {
                ComplexData * complexValue;
                double realValue;
                RationalData * rationalValue;
                int intValue;
            } value;
        };

        using StringData = std::string;
        using VectorData = std::vector<ScamValue>;

        struct ErrorData
        {
            ErrorData() : handled(false) {}

            StringData msg;
            VectorData irritants;
            bool       handled;
            ScamValue  category;
        };

        struct PairData
        {
            ScamValue car;
            ScamValue cdr;
        };

        using ByteVectorData = std::vector<unsigned char>;
        using DictKeyData = std::vector<ScamValue>;
        using DictValueData = std::vector<ScamValue>;

        struct DictData
        {
            DictKeyData   keys;
            DictValueData vals;
        };

        using ClosureDefType = LambdaParser *;
        struct ClosureData
        {
            ClosureDefType parser;
            Env * env;
            bool macrolike;
        };

        struct ClassData
        {
            ClassDefParser * def;
            Env            * capture;
        };

        struct InstanceData
        {
            Env * priv;
            Env * local;
        };

        struct PrimitiveData
        {
            std::string    name;
            PrimFunction   func;
            ScamEngine   * engine;
        };

        struct SpecialFormData
        {
            std::string   name;
            SfFunction    func;
            ScamEngine  * engine;
        };

    private:
        union
        {
            NumericData * numericValue;
            bool boolValue;
            char charValue;
            StringData * strVal;
            ErrorData * errorData;
            PairData * pairValue;
            VectorData * vectorData;
            ByteVectorData * byteVectorData;
            DictData * dictData;
            ClosureData * closureData;
            ClassData * classValue;
            InstanceData * instanceData;
            Continuation * contData;
            PrimitiveData * primitiveData;
            SpecialFormData * specialFormData;
            ScamPort * portData;
        } value;

    public:
        mutable Env * metadata;

    private:
        void assertType(ValueType requiredType);

    public:

        bool & exactFlag();
        ScamValue & realPart();
        ScamValue & imagPart();
        double & realValue();
        int & numPart();
        int & denPart();
        int & intPart();

        bool & boolValue();
        char & charValue();
        StringData & stringValue();

        StringData & errorMessage();
        VectorData & errorIrritants();
        bool & errorHandled();
        ScamValue & errorCategory();

        ScamValue & carValue();
        ScamValue & cdrValue();

        VectorData & vectorData();
        ByteVectorData & byteVectorData();

        DictKeyData & dictKeys();
        DictValueData & dictValues();

        ClosureDefType & closureDef();
        Env *& closureEnv();
        bool & closureMacroLike();

        ClassDefParser *& classDef();
        Env *& classEnv();

        Env *& instancePrivate();
        Env *& instanceLocal();

        Continuation *& contValue();

        std::string & primName();
        PrimFunction & primFunc();
        ScamEngine *& primEngine();

        std::string & sfName();
        SfFunction & sfFunc();
        ScamEngine *& sfEngine();

        ScamPort *& portValue();
    };
}

#endif
