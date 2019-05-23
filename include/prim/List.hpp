#if ! defined(PRIMITIVE_LIST_HPP)
#define PRIMITIVE_LIST_HPP 1

#include "prim/Primitive.hpp"

namespace scam
{
    class List : public Primitive
    {
    private:
        List();

    public:
        static List * makeInstance();
        void applyArgs(ScamValue args, Continuation * cont) override;
    };
}

#endif
