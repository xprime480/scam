#include "util/Validations.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

#include <set>

using namespace scam;
using namespace std;

ScamValue scam::validateClosureArgs(ScamValue args, const char * name)
{
    if ( ! isCons(args) ) {
        ScamValue nameSymbol = ExpressionFactory::makeSymbol(name);
        ScamValue extended = ExpressionFactory::makeCons(nameSymbol, args);
        return ExpressionFactory::makeError("Expected (", name, " args body*)",
                                            "; got: ",
                                            writeValue(extended));
    }

    ScamValue formals = getCar(args);
    return validateFormals(formals);
}


ScamValue scam::validateFormals(ScamValue formals)
{
    ScamValue ok = ExpressionFactory::makeNil();

    if ( isSymbol(formals) || isNil(formals) ) {
        return ok;
    }

    if ( ! isCons(formals) ) {
        return ExpressionFactory::makeError("Formals should be list or symbol",
                                            "; got: ",
                                            writeValue(formals));
    }

    set<string> parms;
    while ( true ) {
        ScamValue arg = getCar(formals);
        if ( ! isSymbol(arg) ) {
            return ExpressionFactory::makeError("Formal parameter should ",
                                                "be a symbol; got: ",
                                                writeValue(arg));
        }

        string name = writeValue(arg);
        if ( parms.end() != parms.find(name) ) {
            return ExpressionFactory::makeError("Symbol cannot appear twice ",
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
                return ExpressionFactory::makeError("Symbol cannot appear ",
                                                    "twice in formals list: ",
                                                    name);
            }
        }

        if ( ! isCons(formals) ) {
            return ExpressionFactory::makeError("Formal parameter should ",
                                                "be a symbol; got: ",
                                                writeValue(formals));
        }
    }

    return ok;
}

