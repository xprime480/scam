#if ! defined(PRIMITIVE_H)
#define PRIMITIVE_H 1

#include "expr/ScamData.hpp"

#include <string>

namespace scam
{
    class Primitive : public ScamData
    {
    public:
        Primitive(std::string const & name);

        virtual void applyArgs(ScamValue args, Continuation * cont) = 0;
    };
}

#endif
