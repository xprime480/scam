#if ! defined(ARGLISTHELPER_H)
#define ARGLISTHELPER_H 1

#include "expr/ExtendedNumeric.hpp"

#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace scam
{
    class Continuation;
    class NumericListParser;
    class RelopsListParser;

    class OpImpl
    {
    public:
        virtual bool
        apply(std::vector<ExtendedNumeric> const & args) const = 0 ;

        virtual bool apply(std::vector<double> const & args) const = 0 ;
        virtual bool apply(std::vector<std::string> const & args) const = 0;
    };

    using NumericalAlgorithm =
        std::function<double(std::vector<double> const &, ExprHandle & state)>;

    extern ExprHandle numericAlgorithm(NumericListParser * parser,
                                       std::string const & context,
                                       NumericalAlgorithm algo);

    extern ExprHandle compareAlgorithm(RelopsListParser * parser,
                                       std::string const & context,
                                       std::shared_ptr<OpImpl> impl);

    extern void failedArgParseMessage(const char * who,
                                      const char * exp,
                                      ExprHandle act,
                                      Continuation * cont);

}

#endif
