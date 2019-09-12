#include "form/AllSpecialForms.hpp"

#include "Backtracker.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/EqualityOps.hpp"
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

const char * scam::spliceValue = "**splicing**";

namespace
{
    extern void condElseClause(ScamValue clauses, Continuation * cont);

    extern bool condApplyClause(ScamValue test,
                                ScamValue clauses,
                                Continuation * cont);

    extern bool condNormalClause(ScamValue test,
                                 ScamValue clauses,
                                 Continuation * cont);

    extern void condEvalClauses(ScamValue clauses, Continuation * cont);
}

void scam::applyAmb(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "amb";

    ObjectParameter  pObj;
    CountedParameter p0(pObj);
    if ( argsToParms(args, name, p0) ) {
        Backtracker * backtracker = ScamEngine::getEngine().getBacktracker();
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Backtracker * newBt =
            mm.make<AmbBacktracker>(p0.value, cont, env, backtracker);
        newBt->run();
    }
}

void scam::applyAnd(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "and";

    ObjectParameter  pObj;
    CountedParameter p0(pObj);
    if ( argsToParms(args, name, p0) ) {
        workQueueHelper<AndWorker>(cont, env, p0.value);
    }
}

void scam::applyApply(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "apply";

    ObjectParameter p0, p1;
    if ( argsToParms(args, name, p0, p1) ) {
        ScamValue sym     = p0.value;
        ScamValue arglist = p1.value;
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Continuation * newCont = mm.make<ApplyOpCont>(arglist, cont, env);

        eval(sym, newCont, env);
    }
}

void scam::applyCallCC(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "call/cc";

    ObjectParameter p0;
    if ( argsToParms(args, name, p0) ) {
        ScamValue body = p0.value;
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Continuation * newCont = mm.make<CallCont>(cont, env);
        eval(body, newCont, env);
    }
}

void scam::applyClassMaker(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "make-class";

    ClassDef def;
    if ( argsToParms(args, name, def) ) {
        ScamValue cls = makeClass(def, env);
        cont->handleValue(cls);
    }
}

void scam::applyCond(ScamValue args, Continuation * cont, Env * env)
{
    ScamEngine & engine = ScamEngine::getEngine();
    static const char * name = "cond";

    if ( length(args) < 1 ) {
        ScamValue err = makeError("Bad Argument list for cond (%{})", args);
        err->errorCategory() = argsCategory;
        engine.handleError(err);
        return;
    }

    while ( ! isNull(args) ) {
        ScamValue arg0 = getCar(args);
        args = getCdr(args);

        ObjectParameter  pObj;
        CountedParameter p0(pObj, 1);
        if ( ! argsToParms(arg0, name, p0) ) {
            break;
        }

        ScamValue first = getCar(p0.value);
        ScamValue rest  = getCdr(p0.value);

        if ( equals(first, makeSymbol("else")) ) {
            condElseClause(rest, cont);
            return;
        }

        if ( (length(rest) > 1) && equals( getCar(rest), makeSymbol("=>")) ) {
            if ( condApplyClause(first, getCdr(rest), cont) ) {
                return;
            }
        }

        else if ( condNormalClause(first, rest, cont) ) {
            return;
        }
    }

    cont->handleValue(makeNull());
}

void scam::applyDefine(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "define";

    ScamValue err = makeError("Bad Argument list for define (%{})", args);
    err->errorCategory() = argsCategory;

    if ( length(args) < 1 ) {
        ScamEngine::getEngine().handleError(err);
        return;
    }

    ScamValue arg0 = getCar(args);
    if ( isSymbol(arg0) ) {
        SymbolParameter p0;
        ObjectParameter p1;

        if ( argsToParms(args, name, p0, p1) ) {
            workQueueHelper<DefineWorker>(p0.value, p1.value, cont, env);
        }
        return;
    }

    else if ( isPair(arg0) ) {
        ScamValue symbol  = getCar(arg0);
        ScamValue formals = getCdr(arg0);
        ScamValue forms   = getCdr(args);
        ScamValue def     = makePair(formals, forms);
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Continuation * c = mm.make<DefineCont>(symbol, cont, env);
        applyLambda(def, c, env);
        return;
    }

    ScamEngine::getEngine().handleError(err);
}

void scam::applyDefineSyntax(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "define-syntax";
    SymbolParameter p0;
    PairParameter   p1;
    if ( argsToParms(args, name, p0, p1) ) {
        ScamValue name  = p0.value;
        ScamValue rules = p1.value;
        SyntaxRules syntax(name, rules, env);
        ScamValue value = makeSyntax(syntax);

        ScamValue test = env->put(name, value);
        if ( isError(test) ) {
            ScamEngine::getEngine().handleError(test);
        }
        else {
            cont->handleValue(makeNothing());
        }
    }
}

void scam::applyIf(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "if";
    ObjectParameter  pObj;
    CountedParameter p0(pObj, 2, 3);
    if ( argsToParms(args, name, p0) ) {
        workQueueHelper<IfWorker>(cont, env, p0.value);
    }
}

void scam::applyLambda(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "lambda";
    LambdaDef lambda;
    if ( argsToParms(args, name, lambda) ) {
        ScamValue expr = makeClosure(lambda, env);
        cont->handleValue(expr);
    }
}

void scam::applyLet(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "let";
    LetDef def;
    if ( argsToParms(args, name, def) ) {
        workQueueHelper<LetWorker>(def, cont, env, false);
    }
}

void scam::applyLetRec(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "letrec";
    LetDef def;
    if ( argsToParms(args, name, def) ) {
        workQueueHelper<LetWorker>(def, cont, env, true);
    }
}

void scam::applyLetStar(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "let*";
    LetDef def;
    if ( argsToParms(args, name, def) ) {
        workQueueHelper<LetStarWorker>(def, cont, env);
    }
}

void scam::applyNot(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name { "not" };

    ObjectParameter p0;
    if ( argsToParms(args, name, p0) ) {
        workQueueHelper<NotWorker>(cont, env, p0.value);
    }
}

void scam::applyOr(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "or";

    ObjectParameter  pObj;
    CountedParameter p0(pObj);
    if ( argsToParms(args, name, p0) ) {
        workQueueHelper<OrWorker>(cont, env, p0.value);
    }
}

void scam::applyQuasiQuote(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "quasiquote";

    ObjectParameter p0;
    if ( argsToParms(args, name, p0) ) {
        workQueueHelper<QuasiQuoteWorker>(p0.value, cont, env);
    }
}

void scam::applyQuote(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "quote";

    ObjectParameter p0;
    if ( argsToParms(args, name, p0) ) {
        cont->handleValue(p0.value);
    }
}

void scam::applySetX(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "set!";

    SymbolParameter p0;
    ObjectParameter p1;
    if ( argsToParms(args, name, p0, p1) ) {
        workQueueHelper<AssignWorker>(p0.value, p1.value, cont, env);
    }
}

void scam::applySyntaxExpand(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "syntax-expand";

    ObjectParameter  p0;
    ObjectParameter  pObj;
    CountedParameter p1(pObj);
    if ( argsToParms(args, name, p0, p1) ) {
        ScamValue value = ScamEngine::getEngine().eval(p0.value);
        if ( isSyntax(value) ) {
            SyntaxRules & syntax = value->syntaxRules();
            ScamValue result = syntax.expandSyntax(p1.value);
            if ( ! isNothing(result) ) {
                cont->handleValue(result);
            }
        }
        else {
            ScamValue err = makeError("expecting syntax got %{0}", p0.value);
            err->errorCategory() = syntaxCategory;
            ScamEngine::getEngine().handleError(err);
        }
    }
}

void scam::applyUndefine(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "undefine";

    SymbolParameter p0;
    if ( argsToParms(args, name, p0) ) {
        workQueueHelper<UndefineWorker>(p0.value, cont, env);
    }
}

ScamValue scam::safeCons(ScamValue expr)
{
    if ( isPair(expr) ) {
        return expr;
    }
    return makeList(expr);
}

namespace
{
    void condElseClause(ScamValue clauses, Continuation * cont)
    {
        ScamEngine & engine = ScamEngine::getEngine();

        if ( isNull(clauses) ) {
            ScamValue err =
                makeError("else clause requires at least one result form");
            err->errorCategory() = syntaxCategory;
            engine.handleError(err);
            return;
        }

        condEvalClauses(clauses, cont);
    }

    bool
    condApplyClause(ScamValue test, ScamValue clauses, Continuation * cont)
    {
        ScamEngine & engine = ScamEngine::getEngine();

        if ( 1 != length(clauses) ) {
            ScamValue err =
                makeError("=> clause requires exactly one result form",
                          clauses);
            err->errorCategory() = syntaxCategory;
            engine.handleError(err);
            return true;
        }

        ScamValue testValue = engine.eval(test);
        if ( isUnhandledError(testValue) ) {
            engine.handleError(testValue);
            return true;
        }
        else if ( ! truth(testValue) ) {
            return false;
        }

        ScamValue form   = getCar(clauses);
        ScamValue proc   = engine.eval(form);
        if ( isUnhandledError(proc) ) {
            engine.handleError(proc);
        }
        else {
	    ScamValue result = engine.apply(proc, makeList(testValue));
	    if ( isUnhandledError(result) ) {
		engine.handleError(result);
	    }
	    else {
		cont->handleValue(result);
	    }
        }

        return true;
    }

    bool
    condNormalClause(ScamValue test, ScamValue clauses, Continuation * cont)
    {
        ScamEngine & engine = ScamEngine::getEngine();

        ScamValue testValue = engine.eval(test);
        if ( isUnhandledError(testValue) ) {
            engine.handleError(testValue);
            return true;
        }
        else if ( ! truth(testValue) ) {
            return false;
        }
        else if ( isNull(clauses) ) {
            cont->handleValue(testValue);
            return true;
        }

        condEvalClauses(clauses, cont);
        return true;
    }

    void condEvalClauses(ScamValue clauses, Continuation * cont)
    {
        ScamEngine & engine = ScamEngine::getEngine();

        ScamValue last = makeNull();

        while ( ! isNull(clauses) ) {
            last = engine.eval(getCar(clauses));
            if ( isUnhandledError(last) ) {
                break;
            }
            clauses = getCdr(clauses);
        }

        if ( isUnhandledError(last) ) {
            engine.handleError(last);
            return;
        }
        else {
            cont->handleValue(last);
        }
    }
}
