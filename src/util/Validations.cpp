#include "util/Validations.hpp"

#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <set>

using namespace scam;
using namespace std;

ExprHandle scam::validateClosureArgs(ExprHandle args, const char * name)
{
    if ( ! args->isCons() ) {
        ExprHandle nameSymbol = ExpressionFactory::makeSymbol(name);
        ExprHandle extended = ExpressionFactory::makeCons(nameSymbol, args);
        return ExpressionFactory::makeError("Expected (", name, " args body*)",
                                            "; got: ",
                                            ExprWriter::write(extended));
    }

    ExprHandle formals = args->getCar();
    return validateFormals(formals);
}


ExprHandle scam::validateFormals(ExprHandle formals)
{
    ExprHandle ok = ExpressionFactory::makeNil();

    if ( formals->isSymbol() || formals->isNil() ) {
        return ok;
    }

    if ( ! formals->isCons() ) {
        return ExpressionFactory::makeError("Formals should be list or symbol",
                                            "; got: ",
                                            ExprWriter::write(formals));
    }

    set<string> parms;
    while ( true ) {
        ExprHandle arg = formals->getCar();
        if ( ! arg->isSymbol() ) {
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
        if ( formals->isNil() ) {
            break;
        }

        if ( formals->isSymbol() ) {
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

        if ( ! formals->isCons() ) {
            return ExpressionFactory::makeError("Formal parameter should ",
                                                "be a symbol; got: ",
                                                ExprWriter::write(formals));
        }
    }

    return ok;
}

