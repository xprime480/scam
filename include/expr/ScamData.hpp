#if ! defined(SCAMDATA_HPP)
#define SCAMDATA_HPP 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"
#include "util/ClassDef.hpp"
#include "util/LambdaDef.hpp"

#include <functional>
#include <string>
#include <vector>

namespace scam
{
    class ScamPort;
    class SyntaxRules;

    using SfFunction   = std::function<void(ScamValue, Continuation *, Env *)>;
    using PrimFunction = std::function<void(ScamValue, Continuation *)>;

    struct ScamData : public ManagedObject
    {
    public:
        explicit ScamData(DataTagType type, bool managed = true);
        ~ScamData();

        ScamData(const ScamData &) = delete;
        ScamData operator=(const ScamData &) = delete;
        ScamData(ScamData &&) = delete;
        ScamData operator=(ScamData &&) = delete;

        static ScamData * makeInstance(DataTagType type, bool managed = true);

        void mark() override final;

        void makeImmutable();
        bool isImmutable() const;

        ScamValue setMeta(std::string const & key, ScamValue value) const;
        ScamValue hasMeta(std::string const & key) const;
        ScamValue getMeta(std::string const & key) const;

        /*
         * tags and types for numeric types
         */
        class NaNType {};
        class NegInfType {};
        class PosInfType {};

        constexpr static DataTagType Complex        { 1 << 0 };
        constexpr static DataTagType Real           { 1 << 1 };
        constexpr static DataTagType Rational       { 1 << 2 };
        constexpr static DataTagType Integer        { 1 << 3 };
        constexpr static DataTagType NaN            { 1 << 4 };
        constexpr static DataTagType NegInf         { 1 << 5 };
        constexpr static DataTagType PosInf         { 1 << 6 };
        constexpr static DataTagType SpecialNumeric { NaN | NegInf | PosInf };
        constexpr static DataTagType RationalTypes  { Rational | Integer };
        constexpr static DataTagType RealNumTypes   { Real | RationalTypes };
        constexpr static DataTagType RealTypes      { RealNumTypes | SpecialNumeric };
        constexpr static DataTagType ComplexTypes   { Complex | RealNumTypes };
        constexpr static DataTagType Numeric        { ComplexTypes | RealTypes };

        /*
         * tags and types for other atoms
         */
        constexpr static DataTagType Nothing      { 1 << 7 };
        constexpr static DataTagType Null         { 1 << 8 };
        constexpr static DataTagType Boolean      { 1 << 9 };
        constexpr static DataTagType Character    { 1 << 10 };
        constexpr static DataTagType Symbol       { 1 << 11 };
        constexpr static DataTagType Keyword      { 1 << 12 };
        constexpr static DataTagType String       { 1 << 13 };
        constexpr static DataTagType Error        { 1 << 14 };
        constexpr static DataTagType Pair         { 1 << 15 };
        constexpr static DataTagType Vector       { 1 << 16 };
        constexpr static DataTagType ByteVector   { 1 << 17 };
        constexpr static DataTagType Dict         { 1 << 18 };
        constexpr static DataTagType Closure      { 1 << 19 };
        constexpr static DataTagType Class        { 1 << 20 };
        constexpr static DataTagType Instance     { 1 << 21 };
        constexpr static DataTagType Cont         { 1 << 22 };

        constexpr static DataTagType StringLike   { Symbol | Keyword | String };

        constexpr static DataTagType Procedure    { Closure | Class | Instance };

        constexpr static DataTagType Primitive   { 1 << 23 };
        constexpr static DataTagType SpecialForm { 1 << 24 };

        constexpr static DataTagType Applicable  { Dict | Procedure | Primitive | SpecialForm | Cont };

        constexpr static DataTagType Port    { 1 << 25 };
        constexpr static DataTagType Eof     { 1 << 26 };

        constexpr static DataTagType Syntax  { 1 << 27 };
        constexpr static DataTagType ScamEnv { 1 << 28 };

        constexpr static DataTagType Placeholder { 1 << 29 };
        constexpr static DataTagType Multiple    { 1 << 30 };

        /**
         * member data
         */
        const DataTagType type;

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

        using ClosureDefType = LambdaDef;

        struct ClosureData
        {
            ClosureDefType lambda;
            Env          * env;
        };

        struct ClassData
        {
            ClassDef def;
            Env    * capture;
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
        };

        struct SpecialFormData
        {
            std::string   name;
            SfFunction    func;
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
            SyntaxRules * syntaxData;
            Env * envData;
            ScamValue valueData;
        } value;

    public:
        mutable Env * metadata;

    private:
        void assertType(DataTagType requiredType);

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

        ClassDef & classDef();
        Env *& classEnv();

        Env *& instancePrivate();
        Env *& instanceLocal();

        Continuation *& contValue();

        std::string & primName();
        PrimFunction & primFunc();

        std::string & sfName();
        SfFunction & sfFunc();

        ScamPort *& portValue();

        SyntaxRules & syntaxRules();

        Env *& envValue();

        ScamValue & placeholderValue();

        VectorData & multipleValues();
    };
}

#endif
