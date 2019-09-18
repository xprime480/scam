#include "form/AllSpecialForms.hpp"

#include "Backtracker.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "form/CondWorker.hpp"
#include "form/DefineCont.hpp"
#include "form/Helpers.hpp"
#include "form/SyntaxRules.hpp"
#include "prim/EquivOps.hpp"
#include "util/ClassDef.hpp"
#include "util/LetDef.hpp"
#include "util/MemoryManager.hpp"
#include "util/Parameter.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

const char * scam::spliceValue = "**splicing**";

namespace
{
    extern void
    caseElseClause(ScamValue key, ScamValue forms, Continuation * cont);

    extern bool caseNormalClause(ScamValue key,
                                 ScamValue values,
                                 ScamValue forms,
                                 Continuation * cont);

    extern bool keyInValues(ScamValue key, ScamValue values);

    extern void
    caseCommonExec(ScamValue key, ScamValue forms, Continuation * cont);

    extern void condElseClause(ScamValue clauses, Continuation * cont);

    extern void condEvalClauses(ScamValue clauses, Continuation * cont);

    extern void
    commonApplyClause(ScamValue form, ScamValue arg, Continuation * cont);

    extern ScamValue evaluateFeatures(ScamValue features);

    extern bool checkForNullElse(ScamValue clauses);
    extern bool checkForExcessApplyClauses(ScamValue clauses);
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

void scam::applyCase(ScamValue args, Continuation * cont, Env * env)
{
    ScamEngine & engine = ScamEngine::getEngine();
    static const char * name = "cond";

    ObjectParameter  p0;
    ListParameter    pList;
    CountedParameter p1(pList, 1);
    if ( ! argsToParms(args, name, p0, p1) ) {
        return;
    }

    ScamValue key = engine.eval(p0.value);
    if ( isUnhandledError(key) ) {
        engine.handleError(key);
        return;
    }

    ScamValue clauses = p1.value;
    while ( ! isNull(clauses) ) {
        ScamValue clause = getCar(clauses);
        clauses = getCdr(clauses);

        ObjectParameter  pObj;
        CountedParameter p0(pObj, 2);
        if ( ! argsToParms(clause, name, p0) ) {
            return;
        }

        ScamValue data = getCar(p0.value);
        ScamValue forms = getCdr(p0.value);

        if ( equals(data, makeSymbol("else")) ) {
            caseElseClause(key, forms, cont);
            return;
        }

        if ( (! isList(data)) || (length(data) < 1) ) {
            ScamValue err =
                makeError("case clause expects list of values", data);
            err->errorCategory() = argsCategory;
            engine.handleError(err);
            return;
        }

        if ( caseNormalClause(key, data, forms, cont) ) {
            return;
        }
    }

    cont->handleValue(makeNull());
}

void scam::applyCondExpand(ScamValue args, Continuation * cont, Env * env)
{
    ScamEngine & engine = ScamEngine::getEngine();
    static const char * name = "cond-expand";

    if ( length(args) < 1 ) {
        ScamValue err =
            makeError("Bad Argument list for cond-expand (%{0})", args);
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

        ScamValue features = getCar(p0.value);
        ScamValue clauses  = getCdr(p0.value);

        if ( equals(features, makeSymbol("else")) ) {
            condElseClause(clauses, cont);
            return;
        }

        ScamValue status = evaluateFeatures(features);
        if ( isUnhandledError(status) ) {
            engine.handleError(status);
            return;
        }
        else if ( truth(status) ) {
            condEvalClauses(clauses, cont);
            return;
        }
    }

    cont->handleValue(makeNull());
}

void scam::applyCond(ScamValue args, Continuation * cont, Env * env)
{
    if ( length(args) < 1 ) {
        ScamValue err = makeError("Bad Argument list for cond (%{0})", args);
        err->errorCategory() = argsCategory;
        ScamEngine::getEngine().handleError(err);
        return;
    }

    workQueueHelper<CondWorker>(args, cont, env);
}

void scam::applyDefine(ScamValue args, Continuation * cont, Env * env)
{
    static const char * name = "define";

    ScamValue err = makeError("Bad Argument list for define (%{0})", args);
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
    void caseElseClause(ScamValue key, ScamValue forms, Continuation * cont)
    {
        if ( checkForNullElse(forms) ) {
            return;
        }

        caseCommonExec(key, forms, cont);
    }

    bool caseNormalClause(ScamValue key,
                          ScamValue values,
                          ScamValue forms,
                          Continuation * cont)
    {
        if ( keyInValues(key, values) ) {
            caseCommonExec(key, forms, cont);
            return true;
        }

        return false;
    }

    bool keyInValues(ScamValue key, ScamValue values)
    {
        while ( ! isNull(values) ) {
            ScamValue value = getCar(values);
            if ( doEqv(key, value) ) {
                return true;
            }
            values = getCdr(values);
        }

        return false;
    }

    void caseCommonExec(ScamValue key, ScamValue forms, Continuation * cont)
    {
        ScamValue first = getCar(forms);
        if ( equals(first, makeSymbol("=>")) ) {
            ScamValue clauses = getCdr(forms);
            if ( checkForExcessApplyClauses(clauses) ) {
                return;
            }
            ScamValue clause = getCar(clauses);
            commonApplyClause(clause, key, cont);
        }
        else {
            condElseClause(forms, cont);
        }
    }

    void condElseClause(ScamValue clauses, Continuation * cont)
    {
        if ( checkForNullElse(clauses) ) {
            return;
        }

        condEvalClauses(clauses, cont);
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

    void commonApplyClause(ScamValue form, ScamValue arg, Continuation * cont)
    {
        ScamEngine & engine = ScamEngine::getEngine();

        ScamValue proc = engine.eval(form);
        if ( isUnhandledError(proc) ) {
            engine.handleError(proc);
        }
        else {
            ScamValue result = engine.apply(proc, makeList(arg));
            if ( isUnhandledError(result) ) {
                engine.handleError(result);
            }
            else {
                cont->handleValue(result);
            }
        }
    }

    ScamValue evaluateFeatures(ScamValue features)
    {
        ScamEngine & engine = ScamEngine::getEngine();

        SymbolParameter  p0;
        ObjectParameter  pObj;
        CountedParameter p1(pObj);
        ScamValue check = argsToParmsMsg(features, p0, p1);
        if ( isUnhandledError(check) ) {
            return check;
        }

        ScamValue rv = makeBoolean(false);
        ScamValue f = p0.value;
        ScamValue arg = p1.value;

        if ( equals(f, makeSymbol("library")) ) {
            if ( 1 != length(arg) ) {
                rv = makeError("library requires exactly one argument %{0}",
                               arg);
                rv->errorCategory() = argsCategory;
            }
            else {
                ScamValue result = engine.findLibrary(getCar(arg));
                if ( isUnhandledError(result) ) {
                    rv = result;
                }
                else {
                    rv = makeBoolean(! isNothing(result) );
                }
            }
        }
        else if ( equals(f, makeSymbol("and")) ) {
            rv = makeBoolean(true);
            while ( ! isNull(arg) ) {
                ScamValue result = evaluateFeatures(getCar(arg));
                if ( isUnhandledError(result) || ! truth(result) ) {
                    rv = result;
                    break;
                }
                arg = getCdr(arg);
            }
        }
        else if ( equals(f, makeSymbol("or")) ) {
            while ( ! isNull(arg) ) {
                ScamValue result = evaluateFeatures(getCar(arg));
                if ( isUnhandledError(result) || truth(result) ) {
                    rv = result;
                    break;
                }
                arg = getCdr(arg);
            }
        }
        else if ( equals(f, makeSymbol("not")) ) {
            if ( 1 != length(arg) ) {
                rv = makeError("not requires exactly one argument %{0}", arg);
                rv->errorCategory() = argsCategory;
            }
            else {
                ScamValue result = evaluateFeatures(getCar(arg));
                if ( isUnhandledError(result) ) {
                    rv = result;
                }
                else {
                    rv = makeBoolean(! truth(result));
                }
            }
        }
        else {
            rv = makeError("unknown cond-expand directive %{0}", arg);
            rv->errorCategory() = argsCategory;
        }

        return rv;
    }

    bool checkForNullElse(ScamValue clauses)
    {
        if ( isNull(clauses) ) {
            ScamValue err =
                makeError("else clause requires at least one result form");
            err->errorCategory() = syntaxCategory;
            ScamEngine::getEngine().handleError(err);
            return true;
        }

        return false;
    }

    bool checkForExcessApplyClauses(ScamValue clauses)
    {
        if ( 1 != length(clauses) ) {
            ScamValue err =
                makeError("=> clause requires exactly one result form",
                          clauses);
            err->errorCategory() = syntaxCategory;
            ScamEngine::getEngine().handleError(err);
            return true;
        }

        return false;
    }
}
