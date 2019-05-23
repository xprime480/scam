#include "prim/EqualP.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
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

void EqualP::applyArgs(ExprHandle args, Continuation * cont)
{
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont);
    }
    else {
        equal_p_impl(parser, cont);
    }
}

bool EqualP::equals(ConstExprHandle expr) const
{
    EqualP const * that = dynamic_cast<EqualP const *>(expr);
    return ( that && (ExprWriter::write(this) == ExprWriter::write(that)) );
}

namespace
{
    static ExprHandle const yes = ExpressionFactory::makeBoolean(true);
    static ExprHandle const no  = ExpressionFactory::makeBoolean(false);

    extern bool compare_all(ListParser * parser);

    template <typename MapFn>
    ExprHandle apply_map(ListParser * parser, MapFn mapper)
    {
        const size_t count = parser->size();
        vector<ExprHandle> mapped;

        for ( size_t idx = 0 ; idx < count ; ++idx ) {
            ExprHandle item = parser->get(idx);
            ExprHandle tmp  = mapper(item);
            mapped.push_back(tmp);
        }

        return ExpressionFactory::makeList(mapped);
    }

    template <typename MapFn, typename ReduceFn>
    ExprHandle map_reduce(ListParser * parser, MapFn mapper, ReduceFn reducer)
    {
        ExprHandle mapped = apply_map(parser, mapper);
        ExprHandle reduced = reducer(mapped);
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

    ExprHandle any(ExprHandle args)
    {
        const size_t len = args->length();
        for ( size_t idx = 0 ; idx < len ; ++idx ) {
            if ( TypePredicates::truth(args->nthcar(idx)) ) {
                return yes;
            }
        }
        return no;
    }

    bool has_nulls(ListParser * parser, Continuation * cont)
    {
        auto fn = [](ExprHandle arg) -> ExprHandle {
            return ExpressionFactory::makeBoolean(TypePredicates::isNull(arg));
        };
        ExprHandle answer = map_reduce(parser, fn, any);
        return TypePredicates::truth(answer);
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
        ExprHandle first = parser->get(0);

        for ( size_t idx = 1 ; idx < len ; ++idx ) {
            ExprHandle op = parser->get(idx);
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
