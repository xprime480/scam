#if ! defined(PRIMITIVE_INCLUDE_H)
#define PRIMITIVE_INCLUDE_H 1

#include "prim/Primitive.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class Include : public Primitive
    {
    private:
        Include(ScamEngine * engine);

    public:
        static Include * makeInstance(ScamEngine * engine);
        void applyArgs(ScamValue args, Continuation * cont) override;

    private:
        ScamEngine * engine;
    };
}

#endif
