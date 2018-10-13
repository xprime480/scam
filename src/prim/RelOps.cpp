
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

    class RelOpImplBase : public OpImpl
    {
    public:
    };

    class EqOpImpl : public RelOpImplBase
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
            const unsigned len = args.size();

            if ( len < 2 ) {
                return true;
            }

            const unsigned lim = len - 1;
            for ( unsigned idx = 0 ; idx < lim ; ++idx ) {
                if ( args[idx] != args[idx+1] ) {
                    return false;
                }
            }

            return true;
        }
    };

    static const shared_ptr<OpImpl> dummyDef =
        make_shared<DummyOpImpl>();

    static const shared_ptr<OpImpl> eqDef = make_shared<EqOpImpl>();
}

#define CMP_OP_DEFINE(Name, Impl)                                \
    Name::Name()                                         \
        : CompareOp(#Name, Impl) {}

CMP_OP_DEFINE(Eq, eqDef);
CMP_OP_DEFINE(Ne, dummyDef);
CMP_OP_DEFINE(Lt, dummyDef);
CMP_OP_DEFINE(Le, dummyDef);
CMP_OP_DEFINE(Gt, dummyDef);
CMP_OP_DEFINE(Ge, dummyDef);

#undef CMP_OP_DEFINE
