#if ! defined(PARAMETER_HPP)
#define PARAMETER_HPP 1

#include "ScamFwd.hpp"
#include "value/TypePredicates.hpp"

#include <functional>

namespace scam
{
    using ParameterFilter = std::function<bool(ScamValue)>;

    struct Parameter
    {
        Parameter();
        virtual ~Parameter();
        virtual ScamValue transform(ScamValue args) = 0;
        bool valid;
    };

    struct ObjectParameter : public Parameter
    {
        ObjectParameter();
        ScamValue transform(ScamValue args) override;
        ScamValue value;
    };

    struct FilteredParameter : public ObjectParameter
    {
        ScamValue transform(ScamValue args) override;

    protected:
        virtual ScamValue check() = 0;
    };

    struct NumericParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct IntegerParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct CharacterParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct SymbolParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct KeywordParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct StringParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct ErrorParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct PairParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct VectorParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct DictParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct ApplicableParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct PortParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct EnvParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;
    };

    struct CountParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;

    private:
        ScamValue outOfRange();
    };

    struct IndexLikeParameter : public ObjectParameter
    {
        IndexLikeParameter(ObjectParameter & referant);
        ScamValue transform(ScamValue args) override;

    protected:
        virtual ScamValue inbounds(int idx, int ref) = 0;

    private:
        ScamValue checkIndexNature(ScamValue args);
        ScamValue checkReferences(ScamValue arg);

        ScamValue invalidReferant();
        ScamValue outOfRange(int idx);

        ObjectParameter & referant;
    };

    struct IndexParameter : public IndexLikeParameter
    {
        IndexParameter(ObjectParameter & referant);

    protected:
        ScamValue inbounds(int idx, int ref) override;
    };

    struct StartIndexParameter : public IndexLikeParameter
    {
        StartIndexParameter(ObjectParameter & referant);

    protected:
        ScamValue inbounds(int idx, int ref) override;
    };

    struct EndIndexParameter : public IndexLikeParameter
    {
        EndIndexParameter(ObjectParameter & referant,
                          StartIndexParameter & start);

    protected:
        ScamValue inbounds(int idx, int ref) override;

    private:
        ScamValue getStartIndex();

        ScamValue invalidStart();
        ScamValue indexOutOfOrder(int idx0, int idx1);

        StartIndexParameter & start;
    };

    struct ListParameter : public FilteredParameter
    {
    protected:
        ScamValue check() override;

    private:
        ScamValue inexactError();
        ScamValue outOfRange();
    };

    struct MutableParameter : public ObjectParameter
    {
        MutableParameter(ObjectParameter & itemizer);
        ScamValue transform(ScamValue args) override;

    private:
        ScamValue mutableError();

        ObjectParameter & itemizer;
    };

    struct OptionalParameter : public ObjectParameter
    {
        OptionalParameter(ObjectParameter & itemizer);
        ScamValue transform(ScamValue args) override;

        bool found;

    private:
        ObjectParameter & itemizer;
    };

    struct CountedParameter : public ObjectParameter
    {
        CountedParameter(ObjectParameter & itemizer,
                         unsigned min = 0,
                         unsigned max = 9999);

        ScamValue transform(ScamValue args) override;

    private:
        ObjectParameter & itemizer;
        unsigned min;
        unsigned max;

        ScamValue tooFewValues(unsigned count);
    };

    struct ListOfParameter : public ObjectParameter
    {
        ListOfParameter(ObjectParameter & itemizer);

        ScamValue transform(ScamValue args) override;

    private:
        ObjectParameter & itemizer;

        ScamValue noProgress();
    };

    struct AlternativeParameter : public ObjectParameter
    {
        AlternativeParameter(ObjectParameter & choice1,
                             ObjectParameter & choice2);

        ScamValue transform(ScamValue args) override;

    private:
        ObjectParameter & choice1;
        ObjectParameter & choice2;

        ScamValue noMatch();
    };

    extern ScamValue errorCheckMsg(const char * name, ScamValue value);
    extern bool errorCheck(const char * name, ScamValue value);

    template <typename... PS>
    ScamValue argsToParmsMsg(ScamValue args, Parameter & parm, PS & ... ps)
    {
        ScamValue result = parm.transform(args);
        if ( isUnhandledError(result) ) {
            return result;
        }
        return argsToParmsMsg(result, ps...);
    }

    template <typename... PS>
    bool argsToParms(ScamValue args,
                     const char * name,
                     Parameter & parm,
                     PS & ... ps)
    {
        ScamValue result = argsToParmsMsg(args, parm, ps...);
        return errorCheck(name, result);
    }

    ScamValue argsToParmsMsg(ScamValue args);
    bool argsToParms(ScamValue args, const char * name);
}

#endif
