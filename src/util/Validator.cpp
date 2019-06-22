#include "util/Validator.hpp"

#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

ObjectValidator::ObjectValidator()
    : complete(false)
    , value(makeNothing())
{
}

ObjectValidator::~ObjectValidator()
{
}

bool ObjectValidator::accept(ScamValue value)
{
    if ( complete || isNothing(value) ) {
        return false;
    }

    this->value = value;
    complete = true;
    return true;
}

bool ObjectValidator::isComplete() const
{
    return complete;
}

ScamValue ObjectValidator::get()
{
    return value;
}

///////// * convenience functions * /////////////////

static const ScamValue argsCategory = makeSymbol(":args");

ScamValue scam::matchList(ScamValue args)
{
    if ( 0 != length(args) ) {
        ScamValue err = makeError("argument error: expected 0, got %{0}",
                                  makeInteger(length(args), true));
        err->errorCategory() = argsCategory;
        return err;
    }
    return makeNothing();
}

ScamValue scam::matchList(ScamValue args,
                          ObjectValidator & v0)
{
    if ( 1 != length(args) ) {
        ScamValue err = makeError("argument error: expected 1, got %{0}",
                                  makeInteger(length(args), true));
        err->errorCategory() = argsCategory;
        return err;
    }

    ScamValue arg0 = nthcar(args, 0);
    if ( ! v0.accept(arg0) ) {
        ScamValue err = makeError("argument error: '%{0}' not valid for arg 1",
                                  arg0);
        err->errorCategory() = argsCategory;
        return err;
    }

    return makeNothing();
}

ScamValue scam::matchList(ScamValue args,
                          ObjectValidator & v0,
                          ObjectValidator & v1)
{
    return makeNothing();
}
