#if ! defined(PRIMITIVE_CAR_HPP)
#define PRIMITIVE_CAR_HPP 1

#include "prim/CarCdr.hpp"

namespace scam
{
    class Car : public CarCdr
    {
    private:
        Car();

    public:
        static Car * makeInstance();

    protected:
        void finish(ScamExpr * args, Continuation * cont) override;
    };
}

#endif
