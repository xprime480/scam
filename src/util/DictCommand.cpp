#include "util/DictCommand.hpp"

#include "ErrorCategory.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/SequenceOps.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

ScamValue DictCommand::getOp = makeKeyword(":get", false);
ScamValue DictCommand::putOp = makeKeyword(":put", false);
ScamValue DictCommand::lenOp = makeKeyword(":length", false);
ScamValue DictCommand::remOp = makeKeyword(":remove", false);
ScamValue DictCommand::hasOp = makeKeyword(":has", false);


DictCommand::DictCommand()
{
    reset();
}
void DictCommand::mark()
{
    op->mark();
    key->mark();
    val->mark();
}

ScamValue DictCommand::transform(ScamValue args)
{
    reset();

    ScamValue rv = getOperator(args);
    if ( ! isUnhandledError(rv) ) {
        rv = getOperands(rv);
        if ( ! isUnhandledError(rv) ) {
            valid = true;
        }
    }

    return rv;
}

void DictCommand::reset()
{
    op = key = val = makeNothing();
}

ScamValue DictCommand::getOperator(ScamValue args)
{
    KeywordParameter p0;
    ScamValue rv = p0.transform(args);

    if ( ! isUnhandledError(rv) ) {
        ScamValue value = p0.value;
        if ( equals(value, getOp) ||
             equals(value, putOp) ||
             equals(value, lenOp) ||
             equals(value, remOp) ||
             equals(value, hasOp) ) {
            op = value;
        }
        else {
            rv = unknownOp(value);
        }
    }

    return rv;
}

ScamValue DictCommand::getOperands(ScamValue args)
{
    unsigned argCount = 99999;

    if ( equals(op, putOp) ) {
        argCount = 2;
    }
    else if ( equals(op, getOp) || equals(op, remOp) || equals(op, hasOp) ) {
        argCount = 1;
    }
    else if ( equals(op, lenOp) ) {
        argCount = 0;
    }

    ObjectParameter  pObj;
    CountedParameter p0(pObj, argCount, argCount);

    ScamValue rv = p0.transform(args);
    if ( ! isUnhandledError(rv) ) {
        if ( isNull(rv) ) {
            ScamValue tmp = p0.value;
            switch ( argCount ) {
            case 2:
                val = nthcar(tmp, 1);
                /** fallthrough */
            case 1:
                key = nthcar(tmp, 0);
                /** fallthrough */
            default:
                break;
            }
        }
        else {
            rv = tooManyArgs();
        }
    }

    return rv;
}

ScamValue DictCommand::unknownOp(ScamValue value)
{
    static const char * msg { "Unknown operation for dict: %{0}" };
    ScamValue err = makeError(msg, value);
    err->errorCategory() = argsCategory;
    return err;
}

ScamValue DictCommand::tooManyArgs()
{
    static const char * msg { "Too many operands for dict operation" };
    ScamValue err = makeError(msg);
    err->errorCategory() = argsCategory;
    return err;
}
