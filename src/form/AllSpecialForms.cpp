#include "form/AllSpecialForms.hpp"

#include "Backtracker.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ValueFactory.hpp"
#include "form/Helpers.hpp"
#include "form/SyntaxUtils.hpp"
#include "util/ClassDef.hpp"
#include "util/LetDef.hpp"
#include "util/MemoryManager.hpp"
#include "util/Parameter.hpp"
#include "util/Validations.hpp"

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

    SymbolParameter p0;
    ObjectParameter p1;
    if ( argsToParms(args, engine, name, p0, p1) ) {
        workQueueHelper<DefineWorker>(p0.value, p1.value, cont, env, engine);
    }
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
        ScamValue symbol = p0.value;
        ScamValue rules  = p1.value;
        installSyntax(env, engine, symbol, rules);
        cont->handleValue(makeNothing());
    }
}

void scam::applyEval(ScamValue args,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine)
{
    static const char * name = "eval";

    ObjectParameter p0;
    if ( argsToParms(args, engine, name, p0) ) {
        Continuation * finisher =
            standardMemoryManager.make<EvalCont>(cont, env, engine);
        eval(p0.value, finisher, env, engine);
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

void scam::applyMacro(ScamValue args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine)
{
    static const char * name = "macro";
    LambdaDef lambda;
    if ( argsToParms(args, engine, name, lambda) ) {
        ScamValue expr = makeClosure(lambda, env, true);
        cont->handleValue(expr);
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
