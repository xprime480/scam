#if ! defined(PRIMITIVE_CDR_HPP)
#define PRIMITIVE_CDR_HPP 1

#include "prim/CarCdr.hpp"

namespace scam
{
    class Cdr : public CarCdr
    {
    private:
        Cdr();

    public:
        static Cdr * makeInstance();

    protected:
        void finish(ExprHandle cons, Continuation * cont) override;
    };
}

#endif
