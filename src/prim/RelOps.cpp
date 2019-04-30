#include "prim/RelOps.hpp"

#include "Continuation.hpp"
#include "util/ArgListHelper.hpp"

#include <functional>

using namespace scam;
using namespace std;

CompareOp::CompareOp(char const * name, shared_ptr<OpImpl> impl)
    : Primitive(name)
    , impl(impl)
{
}

void CompareOp::applyArgs(ExprHandle args, Continuation * cont)
{
    string const context = toString();
    ExprHandle rv = compareAlgorithm(args, context, impl);
    cont->run(rv);
}


bool CompareOp::equals(ConstExprHandle expr) const
{
    return ( expr && this->toString() == expr->toString() );
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

    template <template <typename T> class Cmp>
    class RelOp : public OpImpl
    {
    public:
        bool apply(vector<double> const & args) const override
        {
            return check(args);
        }

        bool apply(vector<string> const & args) const override
        {
            return check(args);
        }

    private:
        template <typename T>
        bool check(vector<T> const & args) const
        {
            Cmp<T> cmp;

            const unsigned len = args.size();

            if ( len < 2 ) {
                return true;
            }

            const unsigned lim = len - 1;
            for ( unsigned idx = 0 ; idx < lim ; ++idx ) {
                if ( ! cmp(args[idx], args[idx+1]) ) {
                    return false;
                }
            }

            return true;
        }
    };

    static const shared_ptr<OpImpl> eqDef = make_shared<RelOp<equal_to>>();
    static const shared_ptr<OpImpl> neDef = make_shared<RelOp<not_equal_to>>();
    static const shared_ptr<OpImpl> ltDef = make_shared<RelOp<less>>();
    static const shared_ptr<OpImpl> leDef = make_shared<RelOp<less_equal>>();
    static const shared_ptr<OpImpl> gtDef = make_shared<RelOp<greater>>();
    static const shared_ptr<OpImpl> geDef = make_shared<RelOp<greater_equal>>();
}

#define CMP_OP_DEFINE(Name, Impl)                                \
    Name::Name()                                         \
        : CompareOp(#Name, Impl) {}                      \
    Name * Name::makeInstance() { return new Name(); }


CMP_OP_DEFINE(Eq, eqDef);
CMP_OP_DEFINE(Ne, neDef);
CMP_OP_DEFINE(Lt, ltDef);
CMP_OP_DEFINE(Le, leDef);
CMP_OP_DEFINE(Gt, gtDef);
CMP_OP_DEFINE(Ge, geDef);

#undef CMP_OP_DEFINE
