
#include "prim/RelOps.hpp"

#include "Continuation.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

CompareOp::CompareOp(char const * name, shared_ptr<OpImpl> impl)
    : Primitive(name)
    , impl(impl)
{
}

void CompareOp::applyArgs(ExprHandle const & args, ContHandle cont)
{
    string const context = toString();
    ExprHandle rv = compareAlgorithm(args, context, impl);
    cont->run(rv);
}

namespace
{
    class DummyOpImpl : public OpImpl
    {
    public:
        bool apply(vector<double> const & args) const override
        {
            return true;
        }

        bool apply(vector<string> const & args) const override
        {
            return true;
        }
    };

    static const shared_ptr<OpImpl> dummyDef =
        make_shared<DummyOpImpl>();
}

#define CMP_OP_DEFINE(Name) \
    Name::Name()                                         \
        : CompareOp(#Name, dummyDef) {}

CMP_OP_DEFINE(Eq);
CMP_OP_DEFINE(Ne);
CMP_OP_DEFINE(Lt);
CMP_OP_DEFINE(Le);
CMP_OP_DEFINE(Gt);
CMP_OP_DEFINE(Ge);

#undef CMP_OP_DEFINE
