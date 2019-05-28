#include "form/AllSpecialForms.hpp"

#include "Backtracker.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ValueFactory.hpp"
#include "form/Helpers.hpp"
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
        failedArgParseMessage(myName, "(form*)", args, cont);
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
        failedArgParseMessage(myName, "(form*)", args, cont);
        return;
    }

    unsigned pos { 0 };
    workQueueHelper<AndWorker>(cont, env, parser, pos);
}

void scam::applyApply(ScamValue args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine)
{
    static const char * myName = "apply";
    ApplyParser * parser = standardMemoryManager.make<ApplyParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(function (args*))", args, cont);
        return;
    }

    ScamValue sym     = parser->getParsedOp();
    ScamValue arglist = parser->getArgs();
    Continuation * newCont =
        standardMemoryManager.make<ApplyOpCont>(arglist, cont, env);

    eval(sym, newCont, env);
}

void scam::applyAssign(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    static const char * myName = "assign!";
    AssignParser * parser = standardMemoryManager.make<AssignParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(sym expr)", args, cont);
    }
    else {
        workQueueHelper<AssignWorker>(parser, cont, env, engine);
    }
}

void scam::applyCallCC(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    static const char * myName = "call/cc";
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont);
    }
    else {
        ScamValue body = parser->get();
        Continuation * newCont =
            standardMemoryManager.make<CallCont>(cont, env);
        eval(body, newCont, env);
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
        failedArgParseMessage(myName, "(Base (vars*) methods*)", args, cont);
    }
    else {
        ScamValue cls = makeClass(parser, env);
        cont->run(cls);
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
        failedArgParseMessage(myName, "(sym expr)", args, cont);
    }
    else {
        workQueueHelper<DefineWorker>(parser, cont, env, engine);
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
        failedArgParseMessage(myName, "(expr)", args, cont);
    }
    else {
        Continuation * finisher =
            standardMemoryManager.make<EvalCont>(cont, env);
        ScamValue expr = const_cast<ScamValue>(parser->get());
        eval(expr, finisher, env);
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
                              cont);
    }
    else {
        workQueueHelper<IfWorker>(cont, env, parser);
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

    cont->run(expr);
}

void scam::applyLet(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    static const char * myName = "let";
    LetParser * parser = standardMemoryManager.make<LetParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(((sym form)*) form*)", args, cont);
    }
    else {
        workQueueHelper<LetWorker>(parser, cont, env, false);
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
        failedArgParseMessage(myName, "(((sym form)*) form*)", args, cont);
    }
    else {
        workQueueHelper<LetWorker>(parser, cont, env, true);
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
        failedArgParseMessage(myName, "(((sym form)*) form*)", args, cont);
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

    cont->run(expr);
}

void scam::applyNot(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    static const char * myName { "not" };
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont);
    }
    else {
        workQueueHelper<NotWorker>(cont, env, parser);
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
        failedArgParseMessage(myName, "(form*)", args, cont);
    }
    else {
        workQueueHelper<OrWorker>(cont, env, parser, 0u);
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
        failedArgParseMessage(myName, "(form)", args, cont);
    }
    else {
        workQueueHelper<QuasiQuoteWorker>(parser->get(), cont, env);
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
        failedArgParseMessage(myName, "(expr)", args, cont);
    }
    else {
        cont->run(parser->get());
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
        failedArgParseMessage(myName, "(sym)", args, cont);
    }
    else {
        workQueueHelper<UndefineWorker>(parser, cont, env);
    }
}

ScamValue scam::safeCons(ScamValue expr)
{
    if ( isCons(expr) ) {
        return expr;
    }
    return makeList(expr);
}
