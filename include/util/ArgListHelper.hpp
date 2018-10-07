#if ! defined(ARGLISTHELPER_H)
#define ARGLISTHELPER_H 1

#include "expr/ScamExpr.hpp"

#include <functional>
#include <string>
#include <vector>

namespace scam
{
    using NumericalAlgorithm =
        std::function<double(std::vector<double> const &)>;

    extern ExprHandle numericAlgorithm(ExprHandle const & args,
                                       std::string const & context,
                                       NumericalAlgorithm algo);
}

#endif
