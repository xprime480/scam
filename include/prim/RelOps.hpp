#if ! defined(PRIMITIVE_COMPAREOPS_H)
#define PRIMITIVE_COMPAREOPS_H 1

#include "prim/Primitive.hpp"

#include <string>
#include <vector>

namespace scam
{
    class OpImpl;

    class CompareOp : public Primitive
    {
    public:
        CompareOp(char const * name, std::shared_ptr<OpImpl> impl);

        void applyArgs(ExprHandle args, Continuation * cont) override;

        bool equals(ConstExprHandle expr) const override;

    private:
        std::shared_ptr<OpImpl> impl;
    };

#define CMP_OP_DECL(Name) \
    class Name : public CompareOp \
    { \
    private: \
        Name(); \
    public: \
        static Name * makeInstance(); \
    };

    CMP_OP_DECL(Eq);
    CMP_OP_DECL(Ne);
    CMP_OP_DECL(Lt);
    CMP_OP_DECL(Le);
    CMP_OP_DECL(Gt);
    CMP_OP_DECL(Ge);

#undef CMP_OP_DECL
}

#endif
