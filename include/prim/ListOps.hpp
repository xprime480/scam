#if ! defined(PRIMITIVE_LISTOPS_H)
#define PRIMITIVE_LISTOPS_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class List : public Primitive
    {
    public:
        List();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class Cons : public Primitive
    {
    public:
        Cons();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class CarCdr : public Primitive
    {
    public:
        CarCdr(char const * name);

        void applyArgs(ScamExpr * args, ContHandle cont) override;

    protected:
        virtual void finish(ScamExpr * args, ContHandle cont) = 0;
    };

    class Car : public CarCdr
    {
    public:
        Car();

    protected:
        void finish(ScamExpr * args, ContHandle cont) override;
    };

    class Cdr : public CarCdr
    {
    public:
        Cdr();

    protected:
        void finish(ScamExpr * args, ContHandle cont) override;
    };
}

#endif
