#include "prim/EqualP.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "input/ListParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern void equal_p_impl(ListParser * parser, Continuation * cont);
}

static const char * myName = "eq?";

EqualP::EqualP()
    : Primitive(myName)
{
}

EqualP * EqualP::makeInstance()
{
    return new EqualP();
}

void EqualP::applyArgs(ScamValue args, Continuation * cont)
{
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont);
    }
    else {
        equal_p_impl(parser, cont);
    }
}

bool EqualP::equals(ConstScamValue expr) const
{
    EqualP const * that = dynamic_cast<EqualP const *>(expr);
    return ( that && (writeValue(this) == writeValue(that)) );
}

namespace
{
    static ScamValue const yes = ExpressionFactory::makeBoolean(true);
    static ScamValue const no  = ExpressionFactory::makeBoolean(false);

    extern bool compare_all(ListParser * parser);

    template <typename MapFn>
    ScamValue apply_map(ListParser * parser, MapFn mapper)
    {
        const size_t count = parser->size();
        vector<ScamValue> mapped;

        for ( size_t idx = 0 ; idx < count ; ++idx ) {
            ScamValue item = parser->get(idx);
            ScamValue tmp  = mapper(item);
            mapped.push_back(tmp);
        }

        return ExpressionFactory::makeList(mapped);
    }

    template <typename MapFn, typename ReduceFn>
    ScamValue map_reduce(ListParser * parser, MapFn mapper, ReduceFn reducer)
    {
        ScamValue mapped = apply_map(parser, mapper);
        ScamValue reduced = reducer(mapped);
        return reduced;
    }

    bool zero_args(ListParser * parser, Continuation * cont)
    {
        if ( 0 == parser->size() ) {
            cont->run(yes);
            return true;
        }
        return false;
    }

    ScamValue any(ScamValue args)
    {
        const size_t len = length(args);
        for ( size_t idx = 0 ; idx < len ; ++idx ) {
            if ( truth(nthcar(args, idx)) ) {
                return yes;
            }
        }
        return no;
    }

    bool has_nulls(ListParser * parser, Continuation * cont)
    {
        auto fn = [](ScamValue arg) -> ScamValue {
            return ExpressionFactory::makeBoolean(isNull(arg));
        };
        ScamValue answer = map_reduce(parser, fn, any);
        return truth(answer);
    }

    bool one_arg(ListParser * parser, Continuation * cont)
    {
        if ( 1 == parser->size() ) {
            cont->run(yes);
            return true;
        }
        return false;
    }

    bool compare_all(ListParser * parser)
    {
        const size_t len = parser->size();
        ScamValue first = parser->get(0);

        for ( size_t idx = 1 ; idx < len ; ++idx ) {
            ScamValue op = parser->get(idx);
            if ( ! first->equals(op) ) {
                return false;
            }
        }

        return true;
    }

    void equal_p_impl(ListParser * parser, Continuation * cont)
    {
        if ( zero_args(parser, cont) ||
             has_nulls(parser, cont) ||
             one_arg(parser, cont) ) {
            return;
        }
        if ( compare_all(parser) ) {
            cont->run(yes);
        }
        else {
            cont->run(no);
        }
    }
}
