#include "util/Validations.hpp"

#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

#include <set>

using namespace scam;
using namespace std;

ScamValue scam::validateClosureArgs(ScamValue args, const char * name)
{
    if ( ! isCons(args) ) {
        ScamValue nameSymbol = makeSymbol(name);
        ScamValue extended = makeCons(nameSymbol, args);
        return makeErrorExtended("Expected (", name, " args body*)",
                                 "; got: ",
                                 writeValue(extended));
    }

    ScamValue formals = getCar(args);
    return validateFormals(formals);
}


ScamValue scam::validateFormals(ScamValue formals)
{
    ScamValue ok = makeNil();

    if ( isSymbol(formals) || isNil(formals) ) {
        return ok;
    }

    if ( ! isCons(formals) ) {
        return makeErrorExtended("Formals should be list or symbol",
                                 "; got: ",
                                 writeValue(formals));
    }

    set<string> parms;
    while ( true ) {
        ScamValue arg = getCar(formals);
        if ( ! isSymbol(arg) ) {
            return makeErrorExtended("Formal parameter should ",
                                     "be a symbol; got: ",
                                     writeValue(arg));
        }

        string name = writeValue(arg);
        if ( parms.end() != parms.find(name) ) {
            return makeErrorExtended("Symbol cannot appear twice ",
                                     "in formals list: ",
                                     name);
        }

        parms.insert(name);

        formals = getCdr(formals);
        if ( isNil(formals) ) {
            break;
        }

        if ( isSymbol(formals) ) {
            name = writeValue(formals);
            if ( parms.end() == parms.find(name) ) {
                break;
            }
            else {
                return makeErrorExtended("Symbol cannot appear ",
                                         "twice in formals list: ",
                                         name);
            }
        }

        if ( ! isCons(formals) ) {
            return makeErrorExtended("Formal parameter should ",
                                     "be a symbol; got: ",
                                     writeValue(formals));
        }
    }

    return ok;
}

