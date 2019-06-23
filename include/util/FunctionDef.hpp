#if ! defined(FUNCTIONDEF_HPP)
#define FUNCTIONDEF_HPP 1

#include "util/LambdaDef.hpp"

namespace scam
{
    struct FunctionDef : public Parameter
    {
        ScamValue fname;
        LambdaDef lambda;

        FunctionDef();
        void mark() const;

        ScamValue transform(ScamValue args);

    };
};

#endif
