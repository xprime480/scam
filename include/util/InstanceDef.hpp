#if ! defined(INSTANCEDEF_HPP)
#define INSTANCEDEF_HPP 1

#include "ScamFwd.hpp"

#include "util/Parameter.hpp"

namespace scam
{
    struct InstanceDef : public Parameter
    {
        ScamValue name;
        ScamValue forms;

        InstanceDef();
        void mark() const;

        ScamValue transform(ScamValue formals);

    private:
	void reset();
    };
}

#endif





