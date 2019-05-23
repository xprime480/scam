#if ! defined(PRIMITIVE_BEGIN_H)
#define PRIMITIVE_BEGIN_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Begin : public Primitive
    {
    private:
        Begin();

    public:
        /* Should this be private */
        static Begin * makeInstance();

        void applyArgs(ScamValue args, Continuation * cont) override;
    };
}

#endif
