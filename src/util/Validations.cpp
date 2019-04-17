#include "util/Validations.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <set>

using namespace scam;
using namespace std;

ScamExpr * scam::validateClosureArgs(ScamExpr * args, const char * name)
{
    if ( ! args->isCons() ) {
        ScamExpr * nameSymbol = ExpressionFactory::makeSymbol(name);
        ScamExpr * extended = ExpressionFactory::makeCons(nameSymbol, args);
        return ExpressionFactory::makeError("Expected (", name, " args body*)",
                                            "; got: ",
                                            extended->toString());
    }

    ScamExpr * formals = args->getCar();
    return validateFormals(formals);
}


ScamExpr * scam::validateFormals(ScamExpr * formals)
{
    ScamExpr * ok = ExpressionFactory::makeNil();

    if ( formals->isSymbol() || formals->isNil() ) {
        return ok;
    }

    if ( ! formals->isCons() ) {
        return ExpressionFactory::makeError("Formals should be list or symbol",
                                            "; got: ",
                                            formals->toString());
    }

    set<string> parms;
    while ( true ) {
        ScamExpr * arg = formals->getCar();
        if ( ! arg->isSymbol() ) {
            return ExpressionFactory::makeError("Formal parameter should ",
                                                "be a symbol; got: ",
                                                arg->toString());
        }

        string name = arg->toString();
        if ( parms.end() != parms.find(name) ) {
            return ExpressionFactory::makeError("Symbol cannot appear twice ",
                                                "in formals list: ",
                                                name);
        }

        parms.insert(name);

        formals = formals->getCdr();
        if ( formals->isNil() ) {
            break;
        }

        if ( formals->isSymbol() ) {
            name = formals->toString();
            if ( parms.end() == parms.find(name) ) {
                break;
            }
            else {
                return ExpressionFactory::makeError("Symbol cannot appear ",
                                                    "twice in formals list: ",
                                                    name);
            }
        }

        if ( ! formals->isCons() ) {
            return ExpressionFactory::makeError("Formal parameter should ",
                                                "be a symbol; got: ",
                                                formals->toString());
        }
    }

    return ok;
}

