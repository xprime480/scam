#if ! defined(INSTANTIATE_HPP)
#define INSTANTIATE_HPP 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Instantiate : public Primitive
    {
    private:
        Instantiate();

    public:
        static Instantiate * makeInstance();
        void applyArgs(ScamValue args, Continuation * cont) override;

    private:
        static size_t counter;
    };
}

#endif
