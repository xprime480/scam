#include "form/AllSpecialForms.hpp"

#include "Backtracker.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ValueFactory.hpp"
#include "form/Helpers.hpp"
#include "form/SyntaxUtils.hpp"
#include "input/ApplyParser.hpp"
#include "input/ClassDefParser.hpp"
#include "input/CountedListParser.hpp"
#include "input/LambdaParser.hpp"
#include "input/LetParser.hpp"
#include "input/ListParser.hpp"
#include "input/SingletonParser.hpp"
#include "input/SymbolPlusParser.hpp"
#include "input/UndefineParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"
#include "util/Validations.hpp"

using namespace scam;
using namespace std;

ScamValue const scam::spliceTag = makeSymbol("**splicing**", false);

void scam::applyAmb(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    static const char * myName = "amb";

    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont, engine);
        return;
    }

    Backtracker * backtracker = engine->getBacktracker();
    Backtracker * newBt =
        standardMemoryManager.make<AmbBacktracker>(args,
                                                   cont,
                                                   env,
                                                   engine,
                                                   backtracker);
    newBt->run();
}

void scam::applyAnd(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    static const char * myName = "and";
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont, engine);
        return;
    }

    unsigned pos { 0 };
    workQueueHelper<AndWorker>(cont, env, parser, engine, pos);
}

void scam::applyApply(ScamValue args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine)
{
    static const char * myName = "apply";
    ApplyParser * parser = standardMemoryManager.make<ApplyParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(function (args*))", args, cont, engine);
        return;
    }

    ScamValue sym     = parser->getParsedOp();
    ScamValue arglist = parser->getArgs();
    Continuation * newCont =
        standardMemoryManager.make<ApplyOpCont>(arglist, cont, env, engine);

    eval(sym, newCont, env, engine);
}

void scam::applyCallCC(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    static const char * myName = "call/cc";
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont, engine);
    }
    else {
        ScamValue body = parser->get();
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
    static const char * myName = "make-class";
    ClassDefParser * parser = standardMemoryManager.make<ClassDefParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName,
                              "(Base (vars*) methods*)",
                              args,
                              cont,
                              engine);
    }
    else {
        ScamValue cls = makeClass(parser, env);
        cont->handleValue(cls);
    }
}

void scam::applyDefine(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    static const char * myName = "define";
    DefineParser * parser = standardMemoryManager.make<DefineParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(sym expr)", args, cont, engine);
    }
    else {
        workQueueHelper<DefineWorker>(parser, cont, env, engine);
    }
}

void scam::applyDefineSyntax(ScamValue args,
                             Continuation * cont,
                             Env * env,
                             ScamEngine * engine)
{
    static const char * name { "define-syntax" };
    ArgListHelper helper(args);

    ScamValue symbol, rules;
    if ( ! wantSymbol(name, helper, cont, engine, symbol) ) {
        return;
    }
    if ( ! wantPair(name, helper, cont, engine, rules) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    if ( installSyntax(env, engine, symbol, rules) ) {
        cont->handleValue(makeNothing());
    }
}
void scam::applyEval(ScamValue args,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine)
{
    static const char * myName = "eval";
    SingletonParser * parser = getSingletonOfAnythingParser();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(expr)", args, cont, engine);
    }
    else {
        Continuation * finisher =
            standardMemoryManager.make<EvalCont>(cont, env, engine);
        ScamValue expr = const_cast<ScamValue>(parser->get());
        eval(expr, finisher, env, engine);
    }
}

void scam::applyIf(ScamValue args,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine)
{
    static const char * myName = "if";
    CountedListParser * parser = getCountedListOfAnythingParser(2, 3);

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName,
                              "(test then-expr else-expr?)",
                              args,
                              cont,
                              engine);
    }
    else {
        workQueueHelper<IfWorker>(cont, env, engine, parser);
    }
}

void scam::applyLambda(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    LambdaParser * lambda = standardMemoryManager.make<LambdaParser>();

    ScamValue expr = nullptr;
    if ( ! lambda->accept(args) ) {
        expr = validateClosureArgs(args, "lambda");
    }
    else {
        expr = makeClosure(lambda, env);
    }

    cont->handleValue(expr);
}

void scam::applyLet(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    static const char * myName = "let";
    LetParser * parser = standardMemoryManager.make<LetParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName,
                              "(((sym form)*) form*)",
                              args,
                              cont,
                              engine);
    }
    else {
        workQueueHelper<LetWorker>(parser, cont, env, engine, false);
    }
}

void scam::applyLetRec(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    static const char * myName = "letrec";
    LetParser * parser = standardMemoryManager.make<LetParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName,
                              "(((sym form)*) form*)",
                              args,
                              cont,
                              engine);
    }
    else {
        workQueueHelper<LetWorker>(parser, cont, env, engine, true);
    }
}

void scam::applyLetStar(ScamValue args,
                        Continuation * cont,
                        Env * env,
                        ScamEngine * engine)
{
    static const char * myName = "let*";
    LetParser * parser = standardMemoryManager.make<LetParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName,
                              "(((sym form)*) form*)",
                              args,
                              cont,
                              engine);
    }
    else {
        workQueueHelper<LetStarWorker>(parser, cont, env, engine);
    }
}

void scam::applyMacro(ScamValue args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine)
{
    LambdaParser * lambda = standardMemoryManager.make<LambdaParser>();

    ScamValue expr = nullptr;
    if ( ! lambda->accept(args) ) {
        expr = validateClosureArgs(args, "macro");
    }
    else {
        expr = makeClosure(lambda, env, true);
    }

    cont->handleValue(expr);
}

void scam::applyNot(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    static const char * myName { "not" };
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont, engine);
    }
    else {
        workQueueHelper<NotWorker>(cont, env, engine, parser);
    }
}

void scam::applyOr(ScamValue args,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine)
{
    static const char * myName = "or";
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont, engine);
    }
    else {
        workQueueHelper<OrWorker>(cont, env, parser, engine, 0u);
    }
}

void scam::applyQuasiQuote(ScamValue args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
{
    static const char * myName = "quasiquote";
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont, engine);
    }
    else {
        workQueueHelper<QuasiQuoteWorker>(parser->get(), cont, env, engine);
    }
}

void scam::applyQuote(ScamValue args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine)
{
    static const char * myName = "quote";
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(expr)", args, cont, engine);
    }
    else {
        cont->handleValue(parser->get());
    }
}

void scam::applySetX(ScamValue args,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine)
{
    static const char * myName = "set!";
    AssignParser * parser = standardMemoryManager.make<AssignParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(sym expr)", args, cont, engine);
    }
    else {
        workQueueHelper<AssignWorker>(parser, cont, env, engine);
    }
}

void scam::applyUndefine(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine)
{
    static const char * myName = "undefine";
    UndefineParser * parser = standardMemoryManager.make<UndefineParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(sym)", args, cont, engine);
    }
    else {
        workQueueHelper<UndefineWorker>(parser, cont, env, engine);
    }
}

ScamValue scam::safeCons(ScamValue expr)
{
    if ( isPair(expr) ) {
        return expr;
    }
    return makeList(expr);
}
