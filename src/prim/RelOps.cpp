#include "prim/RelOps.hpp"

#include "Continuation.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"
#include "util/Parameter.hpp"
#include "value/TypePredicates.hpp"

#include <functional>

using namespace scam;
using namespace std;

namespace
{
    template <typename T>
    struct Properties
    {
        static bool isNaN(T value) { return false; }
    };

    template <>
    struct Properties<ExtendedNumeric>
    {
        static bool isNaN(ExtendedNumeric value)
        {
            ScamValue v = value.get();
            return scam::isNaN(v);
        }
    };

    class DummyOpImpl : public OpImpl
    {
    public:
        bool apply(std::vector<ExtendedNumeric> const & args) const override
        {
            return true;
        }

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
        bool apply(std::vector<ExtendedNumeric> const & args) const override
        {
            return check(args);
        }

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

            const size_t len = args.size();
            if ( 0 == len ) {
                return true;
            }
            if ( 1 == len ) {
                return ! Properties<T>::isNaN(args[0]);
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

#define CMP_OP_DEFINE(Name, Impl)                                       \
    void scam::apply##Name(ScamValue args, Continuation * cont)         \
    {                                                                   \
        static const char * context = { #Name };                        \
                                                                        \
        if ( isNull(args) ) {                                           \
            cont->handleValue(makeBoolean(true));                       \
            return;                                                     \
        }                                                               \
                                                                        \
        StringParameter pStr;                                           \
        NumericParameter pNum;                                          \
        CountedParameter pStrs(pStr, 1);                                \
        CountedParameter pNums(pNum, 1);                                \
        AlternativeParameter p0(pStrs, pNums);                          \
                                                                        \
        if ( argsToParms(args, context, p0) ) {                         \
            ScamValue rv = compareAlgorithm(p0.value, context, Impl);   \
            cont->handleValue(rv);                                      \
        }                                                               \
    }

CMP_OP_DEFINE(Eq, eqDef);
CMP_OP_DEFINE(Ne, neDef);
CMP_OP_DEFINE(Lt, ltDef);
CMP_OP_DEFINE(Le, leDef);
CMP_OP_DEFINE(Gt, gtDef);
CMP_OP_DEFINE(Ge, geDef);

#undef CMP_OP_DEFINE
