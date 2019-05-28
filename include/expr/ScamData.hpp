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

    using SfFunction =
        std::function<void(ScamValue, Continuation *, Env *, ScamEngine *)>;
    using PrimFunction = SfFunction;

    struct ScamData : public ManagedObject
    {
    public:
        explicit ScamData(unsigned long type, bool managed = true);
        ~ScamData();

        ScamData(const ScamData &) = delete;
        ScamData operator=(const ScamData &) = delete;
        ScamData(ScamData &&) = delete;
        ScamData operator=(ScamData &&) = delete;

        static ScamData * makeInstance(unsigned long type, bool managed = true);
        void mark() const override final;

        void setMeta(std::string const & key, ScamValue value) const;
        bool hasMeta(std::string const & key) const;
        ScamValue getMeta(std::string const & key) const;

        /*
         * tags and types for numeric types
         */
        class NaNType {};
        class NegInfType {};
        class PosInfType {};

        constexpr static unsigned long ComplexBit  { 1 << 0 };
        constexpr static unsigned long RealBit     { 1 << 1 };
        constexpr static unsigned long RationalBit { 1 << 2 };
        constexpr static unsigned long IntegerBit  { 1 << 3 };

        constexpr static unsigned long NaNBit      { 1 << 4 };
        constexpr static unsigned long NegInfBit   { 1 << 5 };
        constexpr static unsigned long PosInfBit   { 1 << 6 };

        constexpr static unsigned long Complex  = ComplexBit;
        constexpr static unsigned long Real     = Complex | RealBit;
        constexpr static unsigned long Rational = Real | RationalBit;
        constexpr static unsigned long Integer  = Rational | IntegerBit;
        constexpr static unsigned long NaN      = Real | NaNBit;
        constexpr static unsigned long NegInf   = Real | NegInfBit;
        constexpr static unsigned long PosInf   = Real | PosInfBit;

        constexpr static unsigned long SpecialNumeric = NaNBit | NegInfBit | PosInfBit;
        constexpr static unsigned long Numeric  = (1 << 7) - 1;

        /*
         * tags and types for other atoms
         */
        constexpr static unsigned long Null         { 1 << 7 };
        constexpr static unsigned long Nil          { 1 << 8 };
        constexpr static unsigned long Boolean      { 1 << 9 };
        constexpr static unsigned long Character    { 1 << 10 };
        constexpr static unsigned long Symbol       { 1 << 11 };
        constexpr static unsigned long Keyword      { 1 << 12 };
        constexpr static unsigned long String       { 1 << 13 };
        constexpr static unsigned long Error        { 1 << 14 };
        constexpr static unsigned long Cons         { 1 << 15 };
        constexpr static unsigned long Vector       { 1 << 16 };
        constexpr static unsigned long ByteVector   { 1 << 17 };
        constexpr static unsigned long Dict         { 1 << 18 };
        constexpr static unsigned long Closure      { 1 << 19 };
        constexpr static unsigned long Class        { 1 << 20 };
        constexpr static unsigned long Instance     { 1 << 21 };
        constexpr static unsigned long Cont         { 1 << 22 };

        constexpr static unsigned long Procedure = Closure | Class | Instance;

        constexpr static unsigned long Primitive   { 1 << 23 };
        constexpr static unsigned long SpecialForm { 1 << 24 };

        constexpr static unsigned long Applicable = Dict | Procedure | Primitive | SpecialForm | Cont;

        /**
         * member data
         */

        const unsigned long type;

        union
        {
            bool boolValue;

            char charValue;

            std::string * strVal;

            struct
            {
                ScamValue car;
                ScamValue cdr;
            }  consValue;

            std::vector<ScamValue> * vectorData;

            std::vector<unsigned char> * byteVectorData;

            struct
            {
                std::vector<ScamValue> * keys;
                std::vector<ScamValue> * vals;
            } dictData;

            struct
            {
                bool exact;
                union
                {
                    struct
                    {
                        ScamValue real;
                        ScamValue imag;
                    }  complexValue;

                    double realValue;

                    struct {
                        int num;
                        int den;
                    } rationalValue;

                    int intValue;
                } value;
            } numericValue ;

            struct
            {
                ClassDefParser * def;
                Env            * capture;
            } classValue;

            struct
            {
                const LambdaParser * parser;
                Env * env;
                bool macrolike;
            } closureData;

            struct
            {
                Env * priv;
                Env * local;
            } instanceData;

            Continuation * contData;

            struct
            {
                std::string * name;
                SfFunction  * func;
                ScamEngine  * engine;
            } specialFormData;

        } value;

        mutable Env * metadata;
    };
}

#define BOOLVAL(data) ((data)->value.boolValue)

#define CHARVAL(data) ((data)->value.charValue)

#define NUMERIC(data) ((data)->value.numericValue)
#define EXACT(data) (NUMERIC(data).exact)

#define REALPART(data) (NUMERIC(data).value.complexValue.real)
#define IMAGPART(data) (NUMERIC(data).value.complexValue.imag)

#define REALVAL(data) (NUMERIC(data).value.realValue)

#define NUMPART(data) (NUMERIC(data).value.rationalValue.num)
#define DENPART(data) (NUMERIC(data).value.rationalValue.den)

#define INTVAL(data) (NUMERIC(data).value.intValue)

#define STRVALP(data) ((data)->value.strVal)
#define STRVAL(data) (*(STRVALP(data)))

#define CAR(data) ((data)->value.consValue.car)
#define CDR(data) ((data)->value.consValue.cdr)

#define VECTORP(data) ((data)->value.vectorData)
#define VECTOR(data) (*(VECTORP(data)))

#define BYTEVECTORP(data) ((data)->value.byteVectorData)
#define BYTEVECTOR(data) (*(BYTEVECTORP(data)))

#define DICTKEYSP(data) ((data)->value.dictData.keys)
#define DICTKEYS(data) (*(DICTKEYSP(data)))
#define DICTVALSP(data) ((data)->value.dictData.vals)
#define DICTVALS(data) (*(DICTVALSP(data)))

#define CLOSUREDEF(data) ((data)->value.closureData.parser)
#define CLOSUREENV(data) ((data)->value.closureData.env)
#define MACROLIKE(data) ((data)->value.closureData.macrolike)

#define CLASSDEF(data) ((data)->value.classValue.def)
#define CLASSENV(data) ((data)->value.classValue.capture)

#define INSTANCEPRIVENV(data) ((data)->value.instanceData.priv)
#define INSTANCELOCALENV(data) ((data)->value.instanceData.local)

#define CONTINUATION(data) ((data)->value.contData)

#define SFNAMEP(data) ((data)->value.specialFormData.name)
#define SFNAME(data) (*SFNAMEP(data))
#define SFFUNCP(data) ((data)->value.specialFormData.func)
#define SFFUNC(data) (*SFFUNCP(data))
#define SFENGINE(data) ((data)->value.specialFormData.engine)

#define PRIMNAME(data) (SFNAME(data))
#define PRIMFUNC(data) (SFFUNC(data))
#define PRIMENGINE(data) (SFENGINE(data))

#endif
