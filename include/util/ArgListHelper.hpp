#if ! defined(ARGLISTHELPER_H)
#define ARGLISTHELPER_H 1

#include "expr/ExtendedNumeric.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace scam
{
    class Continuation;
    class NumericListParser;
    class RelopsListParser;

    using ValuePredicate = std::function<bool(ScamValue)>;

    class ArgListHelper
    {
    public:
        explicit ArgListHelper(ScamValue args);

        ScamValue getStatus();

        ScamValue peek();
        ScamValue getAnyValue(ScamValue & value);

        ScamValue getCharacter(char & c);
        ScamValue getInteger(int & value);
        ScamValue getNonNegativeInteger(int & value);
        ScamValue getString(std::string & value);
        ScamValue getPair(ScamValue & value);

        ScamValue getIndex(int & index, int refParameter);


        ScamValue getZeroPlus(ScamValue & value, ValuePredicate pred);

        ScamValue
        getCount(ScamValue & value, ValuePredicate pred, int min, int max);

        ScamValue getOptional(ScamValue & value, ValuePredicate pred);
        ScamValue getSublistOf(ScamValue & value, ValuePredicate pred);

        ScamValue finish();

    private:
        ScamValue args;
        unsigned int lastRead;
        ScamValue status;

        template <typename T>
        ScamValue getOneArg(T & value,
                            ValuePredicate test,
                            std::function<T(ScamValue)> convert,
                            const char * name)
        {
            ScamValue temp;
            getAnyValue(temp);

            if ( isNothing(status) ) {
                if ( test(temp) ) {
                    value = convert(temp);
                }
                else {
                    status = makeErrorExtended("expected ",
                                               name,
                                               " for parameter ",
                                               lastRead,
                                               " got '",
                                               writeValue(temp),
                                               "'");
                }
            }

            return status;
        }
    };

    class OpImpl
    {
    public:
        virtual bool
        apply(std::vector<ExtendedNumeric> const & args) const = 0 ;

        virtual bool apply(std::vector<double> const & args) const = 0 ;
        virtual bool apply(std::vector<std::string> const & args) const = 0;
    };

    using NumericalAlgorithm =
        std::function<ExtendedNumeric(std::vector<ExtendedNumeric> const &,
                                      ScamValue & state)>;

    extern ScamValue numericAlgorithm(NumericListParser * parser,
                                      std::string const & context,
                                      NumericalAlgorithm algo);

    extern ScamValue compareAlgorithm(RelopsListParser * parser,
                                      std::string const & context,
                                      std::shared_ptr<OpImpl> impl);

    extern void failedArgParseMessage(const char * who,
                                      const char * exp,
                                      ScamValue act,
                                      Continuation * cont);


    /***** **** NEW WORLD CODE *************/
    extern bool wantObject(const char * name,
                           ArgListHelper & helper,
                           Continuation * cont,
                           ScamValue & value);

    extern bool wantChar(const char * name,
                         ArgListHelper & helper,
                         Continuation * cont,
                         char & c);

    extern bool wantNonNegativeInteger(const char * name,
                                       ArgListHelper & helper,
                                       Continuation * cont,
                                       int & count);

    extern bool wantString(const char * name,
                           ArgListHelper & helper,
                           Continuation * cont,
                           std::string & str);

    extern bool wantMutableString(const char * name,
                                  ArgListHelper & helper,
                                  Continuation * cont,
                                  ScamValue & value);

    extern bool wantPair(const char * name,
                         ArgListHelper & helper,
                         Continuation * cont,
                         ScamValue & value);

    extern bool wantMutablePair(const char * name,
                                ArgListHelper & helper,
                                Continuation * cont,
                                ScamValue & value);

    extern bool wantIndex(const char * name,
                          ArgListHelper & helper,
                          Continuation * cont,
                          int & index,
                          int ref);

    template <typename T>
    struct Converter
    {
        using type = std::function<T(ScamValue)>;
    };

    template <typename T>
    T wantOptional(const char * name,
                   ArgListHelper & helper,
                   Continuation * cont,
                   ValuePredicate pred,
                   typename Converter<T>::type convert,
                   T defaultValue)
    {
        ScamValue temp;
        T value = defaultValue;
        (void) helper.getOptional(temp, pred);
        if ( ! isNothing(temp) ) {
            value = convert(temp);
        }

        return value;
    }

    extern bool wantZeroPlus(const char * name,
                             ArgListHelper & helper,
                             Continuation * cont,
                             ScamValue & value,
                             ValuePredicate pred);

    extern bool wantCount(const char * name,
                          ArgListHelper & helper,
                          Continuation * cont,
                          ScamValue & value,
                          ValuePredicate pred,
                          int min,
                          int max);

    extern bool wantSublistOf(const char * name,
                              ArgListHelper & helper,
                              Continuation * cont,
                              ScamValue & value,
                              ValuePredicate pred);

    extern bool finishArgs(const char * name,
                           ArgListHelper & helper,
                           Continuation * cont,
                           const char * msg = nullptr);

    extern size_t length(const std::string & v);

    template <typename Ref>
    ValuePredicate isStartIndexOf(Ref & ref)
    {
        return [&] (ScamValue value) -> bool
        {
            if ( ! isInteger(value) ) {
                return false;
            }
            int val = asInteger(value);
            return ((val >= 0) && (val <= (int)length(ref)));
        };
    }

    template <typename Ref>
    ValuePredicate isEndIndexOf(Ref & ref, int start)
    {
        return [=, &ref] (ScamValue value) -> bool
        {
            if ( ! isInteger(value) ) {
                return false;
            }
            int val = asInteger(value);
            bool rv = (val >= start) && (val <= (int)length(ref));
            return rv;
        };
    }

    extern bool getTwoObjs(ScamValue args,
                           Continuation * cont,
                           const char * name,
                           ScamValue & obj1,
                           ScamValue & obj2);

    extern ScamValue identity(ScamValue value);
}

#endif
