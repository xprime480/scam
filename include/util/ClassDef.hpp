#if ! defined(CLASSDEF_HPP)
#define CLASSDEF_HPP 1

#include "ScamFwd.hpp"
#include "util/FunctionDef.hpp"

#include <vector>

namespace scam
{
    struct ClassDef : public Parameter
    {
        ScamValue base;
        ScamValue vars;
        std::vector<FunctionDef> methods;

        ClassDef();
        void mark() const;

        ScamValue transform(ScamValue args);

    private:
        ScamValue transformMethods(ScamValue defs);
    };
};

#endif
