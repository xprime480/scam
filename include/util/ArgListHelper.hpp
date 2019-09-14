#if ! defined(ARGLISTHELPER_H)
#define ARGLISTHELPER_H 1

#include "expr/ExtendedNumeric.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace scam
{
    class Continuation;

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

    extern ScamValue numericAlgorithm(ScamValue value,
                                      std::string const & context,
                                      NumericalAlgorithm algo);

    extern ScamValue compareAlgorithm(ScamValue value,
                                      std::string const & context,
                                      std::shared_ptr<OpImpl> impl);
}

#endif
