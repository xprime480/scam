#if ! defined(PRIMITIVE_LISTOPS_H)
#define PRIMITIVE_LISTOPS_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class List : public Primitive
    {
    private:
        List();

    public:
        static List * makeInstance();
        void applyArgs(ScamExpr * args, Continuation * cont) override;
    };

    class Cons : public Primitive
    {
    private:
        Cons();

    public:
        static Cons * makeInstance();
        void applyArgs(ScamExpr * args, Continuation * cont) override;
    };

    class CarCdr : public Primitive
    {
    public:
        CarCdr(char const * name);

        void applyArgs(ScamExpr * args, Continuation * cont) override;

    protected:
        virtual void finish(ScamExpr * args, Continuation * cont) = 0;
    };

    class Car : public CarCdr
    {
    private:
        Car();

    public:
        static Car * makeInstance();

    protected:
        void finish(ScamExpr * args, Continuation * cont) override;
    };

    class Cdr : public CarCdr
    {
    private:
        Cdr();

    public:
        static Cdr * makeInstance();

    protected:
        void finish(ScamExpr * args, Continuation * cont) override;
    };
}

#endif
