#include "util/Validations.hpp"

#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"

#include <set>

using namespace scam;
using namespace std;

ScamValue scam::validateClosureArgs(ScamValue args, const char * name)
{
    if ( ! TypePredicates::isCons(args) ) {
        ScamValue nameSymbol = ExpressionFactory::makeSymbol(name);
        ScamValue extended = ExpressionFactory::makeCons(nameSymbol, args);
        return ExpressionFactory::makeError("Expected (", name, " args body*)",
                                            "; got: ",
                                            ExprWriter::write(extended));
    }

    ScamValue formals = args->getCar();
    return validateFormals(formals);
}


ScamValue scam::validateFormals(ScamValue formals)
{
    ScamValue ok = ExpressionFactory::makeNil();

    if ( TypePredicates::isSymbol(formals) || TypePredicates::isNil(formals) ) {
        return ok;
    }

    if ( ! TypePredicates::isCons(formals) ) {
        return ExpressionFactory::makeError("Formals should be list or symbol",
                                            "; got: ",
                                            ExprWriter::write(formals));
    }

    set<string> parms;
    while ( true ) {
        ScamValue arg = formals->getCar();
        if ( ! TypePredicates::isSymbol(arg) ) {
            return ExpressionFactory::makeError("Formal parameter should ",
                                                "be a symbol; got: ",
                                                ExprWriter::write(arg));
        }

        string name = ExprWriter::write(arg);
        if ( parms.end() != parms.find(name) ) {
            return ExpressionFactory::makeError("Symbol cannot appear twice ",
                                                "in formals list: ",
                                                name);
        }

        parms.insert(name);

        formals = formals->getCdr();
        if ( TypePredicates::isNil(formals) ) {
            break;
        }

        if ( TypePredicates::isSymbol(formals) ) {
            name = ExprWriter::write(formals);
            if ( parms.end() == parms.find(name) ) {
                break;
            }
            else {
                return ExpressionFactory::makeError("Symbol cannot appear ",
                                                    "twice in formals list: ",
                                                    name);
            }
        }

        if ( ! TypePredicates::isCons(formals) ) {
            return ExpressionFactory::makeError("Formal parameter should ",
                                                "be a symbol; got: ",
                                                ExprWriter::write(formals));
        }
    }

    return ok;
}

