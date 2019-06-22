#if ! defined(VALIDATOR_HPP)
#define VALIDATOR_HPP 1

#include "ScamFwd.hpp"
#include "expr/ScamData.hpp"

#include "util/GlobalId.hpp"
#include "util/DebugTrace.hpp"
#include "expr/ValueWriter.hpp"

namespace scam
{
    class ObjectValidator
    {
    public:
        ObjectValidator();
        virtual ~ObjectValidator();
        virtual bool accept(ScamValue value);
        ScamValue get();
        virtual bool isComplete() const;

    protected:
        bool complete;
        ScamValue value;
    };

    template <ScamData::ValueType T>
    class TypeValidator : public ObjectValidator
    {
    public:
        bool accept(ScamValue value) override
        {
            GlobalId id;
            scamTrace(id, __FILE__, __LINE__, __FUNCTION__,
                      writeValue(value));

            return ( (0 != (T & value->type)) &&
                     ObjectValidator::accept(value) );
        }
    };

    using NumericValidator     = TypeValidator<ScamData::Numeric>;
    using NothingValidator     = TypeValidator<ScamData::Nothing>;
    using NullValidator        = TypeValidator<ScamData::Null>;
    using BooleanValidator     = TypeValidator<ScamData::Boolean>;
    using CharacterValidator   = TypeValidator<ScamData::Character>;
    using SymbolValidator      = TypeValidator<ScamData::Symbol>;
    using KeywordValidator     = TypeValidator<ScamData::Keyword>;
    using StringValidator      = TypeValidator<ScamData::String>;
    using ErrorValidator       = TypeValidator<ScamData::Error>;
    using PairValidator        = TypeValidator<ScamData::Pair>;
    using VectorValidator      = TypeValidator<ScamData::Vector>;
    using ByteVectorValidator  = TypeValidator<ScamData::ByteVector>;
    using DictValidator        = TypeValidator<ScamData::Dict>;
    using ClosureValidator     = TypeValidator<ScamData::Closure>;
    using ClassValidator       = TypeValidator<ScamData::Class>;
    using InstanceValidator    = TypeValidator<ScamData::Instance>;
    using ContValidator        = TypeValidator<ScamData::Cont>;
    using StringLikeValidator  = TypeValidator<ScamData::StringLike>;
    using ProcedureValidator   = TypeValidator<ScamData::Procedure>;
    using PrimitiveValidator   = TypeValidator<ScamData::Primitive>;
    using SpecialFormValidator = TypeValidator<ScamData::SpecialForm>;
    using ApplicableValidator  = TypeValidator<ScamData::Applicable>;
    using PortValidator        = TypeValidator<ScamData::Port>;
    using EofValidator         = TypeValidator<ScamData::Eof>;

    ///////// * convenience functions * /////////////////

    extern ScamValue matchList(ScamValue args);

    extern ScamValue matchList(ScamValue args,
                               ObjectValidator & v0);

    extern ScamValue matchList(ScamValue args,
                               ObjectValidator & v0,
                               ObjectValidator & v1);
}

#endif
