#include "ScamEngine.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

#include "expr/ScamExpr.hpp"

#include "input/ScamParser.hpp"

#include "output/OutputHandler.hpp"

#include <iostream>

using namespace std;
using namespace scam;

ScamEngine::ScamEngine()
{
}

namespace
{
    class ReplWorker;

    enum class ReplState : unsigned char { READ, EVAL, PRINT, ERROR };

    class ReadContinuation : public Continuation
    {
    public:
        ReadContinuation(ReplWorker & repl);
        void run(ExprHandle expr) const override;

    private:
        ReplWorker & repl;
    };

    class EvalContinuation : public Continuation
    {
    public:
        EvalContinuation(ReplWorker & repl);
        void run(ExprHandle expr) const override;

    private:
        ReplWorker & repl;
    };

    class PrintContinuation : public Continuation
    {
    public:
        PrintContinuation(ReplWorker & repl);
        void run(ExprHandle expr) const override;

    private:
        ReplWorker & repl;
    };

    class ReplWorker : public Worker
    {
    public:
        friend class ReadContinuation;
        friend class EvalContinuation;
        friend class PrintContinuation;

        ReplWorker(ScamParser & parser, OutputHandler & output)
            : parser(parser)
            , output(output)
            , state(ReplState::READ)
        {
        }

        ReplWorker(ReplWorker const &) = default;
        ReplWorker & operator=(ReplWorker const &) = default;
        ReplWorker(ReplWorker &&) = delete;
        ReplWorker & operator=(ReplWorker &&) = delete;

        void run() override
        {
            switch ( state ) {
            case ReplState::READ:
                do_read();
                break;

            case ReplState::EVAL:
                do_eval();
                break;

            case ReplState::PRINT:
                do_print();
                break;

            case ReplState::ERROR:
                do_error();
                break;
            }
        }

    private:
        ScamParser & parser;
        OutputHandler & output;
        ReplState state;
        ExprHandle expr;
        Env env;

        void do_read()
        {
            ContHandle cont = make_shared<ReadContinuation>(*this);
            parser.parseExpr(cont);
        }

        void do_eval()
        {
            ContHandle cont = make_shared<EvalContinuation>(*this);
            expr->eval(cont, env);
        }

        void do_print()
        {
            ContHandle cont = make_shared<PrintContinuation>(*this);
            string value = expr->toString();
            output.handleResult(value);
            cont->run(expr);
        }

        void do_error()
        {
            ContHandle cont = make_shared<PrintContinuation>(*this);
            string value = expr->toString();
            output.handleError(value);
            cont->run(expr);
        }
    };

    ReadContinuation::ReadContinuation(ReplWorker & repl)
        : repl(repl)
    {
    }

    void ReadContinuation::run(ExprHandle expr) const
    {
        if ( ! expr ) {
            return;
        }

        shared_ptr<ReplWorker> replNext = workQueueHelper<ReplWorker>(repl);
        replNext->expr  = expr;

        if ( expr->error() ) {
            replNext->state = ReplState::ERROR;
        }
        else {
            replNext->state = ReplState::EVAL;
        }

    }

    EvalContinuation::EvalContinuation(ReplWorker & repl)
        : repl(repl)
    {
    }

    void EvalContinuation::run(ExprHandle expr) const
    {
        shared_ptr<ReplWorker> replNext = workQueueHelper<ReplWorker>(repl);
        replNext->expr  = expr;

        if ( expr->error() ) {
            replNext->state = ReplState::ERROR;
        }
        else {
            replNext->state = ReplState::PRINT;
        }
    }

    PrintContinuation::PrintContinuation(ReplWorker & repl)
        : repl(repl)
    {
    }

    void PrintContinuation::run(ExprHandle expr) const
    {
        shared_ptr<ReplWorker> replNext = workQueueHelper<ReplWorker>(repl);
        replNext->state = ReplState::READ;
        replNext->expr  = expr;
    }
}

void ScamEngine::repl(Tokenizer & input, OutputHandler & output)
{
    ScamParser parser(input);
    workQueueHelper<ReplWorker>(parser, output);
}

void ScamEngine::extend(std::string const & name,
                        Tokenizer & input,
                        OutputHandler & output)
{
}
