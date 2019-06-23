#if ! defined(LAMBDADEF_HPP)
#define LAMBDADEF_HPP 1

#include "ScamFwd.hpp"

#include "util/Parameter.hpp"

namespace scam
{
    struct LambdaDef : public Parameter
    {
        ScamValue formals;
        ScamValue rest;
        ScamValue forms;

        LambdaDef();
        void mark() const;

        ScamValue transform(ScamValue formals);

    private:
        void reset();
        ScamValue getFormals(ScamValue args);
        ScamValue transformPair(ScamValue args);
        ScamValue getForms(ScamValue args);

        ScamValue badFormals(ScamValue value);
        ScamValue duplicateFormal(ScamValue value);
    };
}

#endif





