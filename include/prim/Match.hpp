#if ! defined(MATCH_HPP)
#define MATCH_HPP 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Match : public Primitive
    {
    private:
        Match();

    public:
        static Match * makeInstance();
        void applyArgs(ScamValue args, Continuation * cont) override;
    };
}

#endif
