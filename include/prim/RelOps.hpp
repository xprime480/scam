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

        void applyArgs(ExprHandle const & args, ContHandle cont) override;

    private:
        std::shared_ptr<OpImpl> impl;
    };

#define CMP_OP_DECL(Name) \
    class Name : public CompareOp \
    { \
    public: \
        Name(); \
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
