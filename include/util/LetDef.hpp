#if ! defined(LETDEF_HPP)
#define LETDEF_HPP 1

#include "ScamFwd.hpp"

#include "util/Parameter.hpp"

namespace scam
{
    struct LetDef : public Parameter
    {
        ScamValue formals;
        ScamValue values;
        ScamValue forms;

        LetDef();
        void mark();

        ScamValue transform(ScamValue formals);

    private:
        void reset();
	
	ScamValue getBindings(ScamValue args);
	ScamValue getForms(ScamValue args);
    };
}

#endif





