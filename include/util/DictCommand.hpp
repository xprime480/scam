#if ! defined(DICTCOMMAND_HPP)
#define DICTCOMMAND_HPP 1

#include "ScamFwd.hpp"

#include "util/Parameter.hpp"

namespace scam
{
    struct DictCommand : public Parameter
    {
        static ScamValue getOp;
        static ScamValue putOp;
        static ScamValue lenOp;
        static ScamValue remOp;
        static ScamValue hasOp;

        ScamValue op;
        ScamValue key;
        ScamValue val;

        DictCommand();
        void mark();

        ScamValue transform(ScamValue formals);

    private:
        void reset();
        ScamValue getOperator(ScamValue args);
        ScamValue getOperands(ScamValue args);

        ScamValue unknownOp(ScamValue value);
        ScamValue tooManyArgs();
    };
}

#endif





