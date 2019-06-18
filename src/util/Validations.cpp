#include "util/Validations.hpp"

#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

#include <set>
#include <sstream>

using namespace scam;
using namespace std;

ScamValue scam::validateClosureArgs(ScamValue args, const char * name)
{
    if ( ! isPair(args) ) {
        ScamValue nameSymbol = makeSymbol(name);
        ScamValue extended = makePair(nameSymbol, args);
        return makeError("Expected (%{0} args body*); got %{1}",
                         nameSymbol,
                         extended);
    }

    ScamValue formals = getCar(args);
    return validateFormals(formals);
}


ScamValue scam::validateFormals(ScamValue formals)
{
    static const char * badTop { "Formals should be list or symbol (%{0})" };

    static const char * nonSymbol
    { "Formal parameter should be a symbol (%{0})" };

    static const char * dupSymbol
    { "Symbol cannot appear twice in formals (%{0})" };

    ScamValue ok = makeNull();

    if ( isSymbol(formals) || isNull(formals) ) {
        return ok;
    }

    if ( ! isPair(formals) ) {
        return makeError(badTop, formals);
    }

    set<string> parms;
    while ( true ) {
        ScamValue arg = getCar(formals);
        if ( ! isSymbol(arg) ) {
            return makeError(nonSymbol, arg);
        }

        string name = writeValue(arg);
        if ( parms.end() != parms.find(name) ) {
            return makeError(dupSymbol, arg);
        }

        parms.insert(name);

        formals = getCdr(formals);
        if ( isNull(formals) ) {
            break;
        }

        if ( isSymbol(formals) ) {
            name = writeValue(formals);
            if ( parms.end() == parms.find(name) ) {
                break;
            }
            else {
                return makeError(dupSymbol, formals);
            }
        }

        if ( ! isPair(formals) ) {
            return makeError(nonSymbol, formals);
        }
    }

    return ok;
}
