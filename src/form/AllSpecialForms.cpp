#include "form/AllSpecialForms.hpp"

#include "Backtracker.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "form/DefineCont.hpp"
#include "form/Helpers.hpp"
#include "form/SyntaxRules.hpp"
#include "util/ClassDef.hpp"
#include "util/LetDef.hpp"
#include "util/MemoryManager.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

ScamValue const scam::spliceTag = makeSymbol("**splicing**", false);

void scam::applyAmb(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    static const char * name = "amb";

    ObjectParameter  pObj;
    CountedParameter p0(pObj);
    if ( argsToParms(args, engine, name, p0) ) {
        Backtracker * backtracker = engine->getBacktracker();
        Backtracker * newBt =
            standardMemoryManager.make<AmbBacktracker>(p0.value,
                                                       cont,
                                                       env,
                                                       engine,
                                                       backtracker);
        newBt->run();
    }
}

void scam::applyAnd(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    static const char * name = "and";

    ObjectParameter  pObj;
    CountedParameter p0(pObj);
    if ( argsToParms(args, engine, name, p0) ) {
        workQueueHelper<AndWorker>(cont, env, p0.value, engine);
    }
}

void scam::applyApply(ScamValue args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine)
{
    static const char * name = "apply";

    ObjectParameter p0, p1;
    if ( argsToParms(args, engine, name, p0, p1) ) {
        ScamValue sym     = p0.value;
        ScamValue arglist = p1.value;
        Continuation * newCont =
            standardMemoryManager.make<ApplyOpCont>(arglist, cont, env, engine);

        eval(sym, newCont, env, engine);
    }
}

void scam::applyCallCC(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    static const char * name = "call/cc";

    ObjectParameter p0;
    if ( argsToParms(args, engine, name, p0) ) {
        ScamValue body = p0.value;
        Continuation * newCont =
            standardMemoryManager.make<CallCont>(cont, env, engine);
        eval(body, newCont, env, engine);
    }
}

void scam::applyClassMaker(ScamValue args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
{
    static const char * name = "make-class";

    ClassDef def;
    if ( argsToParms(args, engine, name, def) ) {
        ScamValue cls = makeClass(def, env);
        cont->handleValue(cls);
    }
}

void scam::applyDefine(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    static const char * name = "define";

    ScamValue err = makeError("Bad Argument list for define (%{})", args);
    err->errorCategory() = argsCategory;

    if ( length(args) < 1 ) {
        engine->handleError(err);
        return;
    }

    ScamValue arg0 = getCar(args);
    if ( isSymbol(arg0) ) {
        SymbolParameter p0;
        ObjectParameter p1;

        if ( argsToParms(args, engine, name, p0, p1) ) {
            workQueueHelper<DefineWorker>(p0.value,
                                          p1.value,
                                          cont,
                                          env,
                                          engine);
        }
        return;
    }

    else if ( isPair(arg0) ) {
        ScamValue symbol  = getCar(arg0);
        ScamValue formals = getCdr(arg0);
        ScamValue forms   = getCdr(args);
        ScamValue def     = makePair(formals, forms);
        Continuation * c =
	    standardMemoryManager.make<DefineCont>(symbol, cont, env, engine);
        applyLambda(def, c, env, engine);
        return;
    }

    engine->handleError(err);
}

void scam::applyDefineSyntax(ScamValue args,
                             Continuation * cont,
                             Env * env,
                             ScamEngine * engine)
{
    static const char * name = "define-syntax";
    SymbolParameter p0;
    PairParameter   p1;
    if ( argsToParms(args, engine, name, p0, p1) ) {
        ScamValue name  = p0.value;
        ScamValue rules = p1.value;
        SyntaxRules syntax(engine, name, rules);
        ScamValue value = makeSyntax(syntax);

        ScamValue test = env->put(name, value);
        if ( isError(test) ) {
            engine->handleError(test);
        }
        else {
            cont->handleValue(makeNothing());
        }
    }
}

void scam::applyIf(ScamValue args,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine)
{
    static const char * name = "if";
    ObjectParameter  pObj;
    CountedParameter p0(pObj, 2, 3);
    if ( argsToParms(args, engine, name, p0) ) {
        workQueueHelper<IfWorker>(cont, env, engine, p0.value);
    }
}

void scam::applyLambda(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    static const char * name = "lambda";
    LambdaDef lambda;
    if ( argsToParms(args, engine, name, lambda) ) {
        ScamValue expr = makeClosure(lambda, env);
        cont->handleValue(expr);
    }
}

void scam::applyLet(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    static const char * name = "let";
    LetDef def;
    if ( argsToParms(args, engine, name, def) ) {
        workQueueHelper<LetWorker>(def, cont, env, engine, false);
    }
}

void scam::applyLetRec(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    static const char * name = "letrec";
    LetDef def;
    if ( argsToParms(args, engine, name, def) ) {
        workQueueHelper<LetWorker>(def, cont, env, engine, true);
    }
}

void scam::applyLetStar(ScamValue args,
                        Continuation * cont,
                        Env * env,
                        ScamEngine * engine)
{
    static const char * name = "let*";
    LetDef def;
    if ( argsToParms(args, engine, name, def) ) {
        workQueueHelper<LetStarWorker>(def, cont, env, engine);
    }
}

void scam::applyNot(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    static const char * name { "not" };

    ObjectParameter p0;
    if ( argsToParms(args, engine, name, p0) ) {
        workQueueHelper<NotWorker>(cont, env, engine, p0.value);
    }
}

void scam::applyOr(ScamValue args,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine)
{
    static const char * name = "or";

    ObjectParameter  pObj;
    CountedParameter p0(pObj);
    if ( argsToParms(args, engine, name, p0) ) {
        workQueueHelper<OrWorker>(cont, env, p0.value, engine);
    }
}

void scam::applyQuasiQuote(ScamValue args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
{
    static const char * name = "quasiquote";

    ObjectParameter p0;
    if ( argsToParms(args, engine, name, p0) ) {
        workQueueHelper<QuasiQuoteWorker>(p0.value, cont, env, engine);
    }
}

void scam::applyQuote(ScamValue args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine)
{
    static const char * name = "quote";

    ObjectParameter p0;
    if ( argsToParms(args, engine, name, p0) ) {
        cont->handleValue(p0.value);
    }
}

void scam::applySetX(ScamValue args,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine)
{
    static const char * name = "set!";

    SymbolParameter p0;
    ObjectParameter p1;
    if ( argsToParms(args, engine, name, p0, p1) ) {
        workQueueHelper<AssignWorker>(p0.value, p1.value, cont, env, engine);
    }
}

void scam::applySyntaxExpand(ScamValue args,
                             Continuation * cont,
                             Env * env,
                             ScamEngine * engine)
{
    static const char * name = "syntax-expand";

    ObjectParameter  p0;
    ObjectParameter  pObj;
    CountedParameter p1(pObj);
    if ( argsToParms(args, engine, name, p0, p1) ) {
        ScamValue value = engine->eval(p0.value);
        if ( isSyntax(value) ) {
            SyntaxRules & syntax = value->syntaxRules();
            ScamValue result = syntax.expandSyntax(p1.value, env, engine);
            if ( ! isNothing(result) ) {
                cont->handleValue(result);
            }
        }
        else {
            ScamValue err = makeError("expecting syntax got %{0}", p0.value);
            err->errorCategory() = syntaxCategory;
            engine->handleError(err);
        }
    }
}

void scam::applyUndefine(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine)
{
    static const char * name = "undefine";

    SymbolParameter p0;
    if ( argsToParms(args, engine, name, p0) ) {
        workQueueHelper<UndefineWorker>(p0.value, cont, env, engine);
    }
}

ScamValue scam::safeCons(ScamValue expr)
{
    if ( isPair(expr) ) {
        return expr;
    }
    return makeList(expr);
}
