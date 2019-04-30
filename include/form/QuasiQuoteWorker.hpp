#if ! defined(QUASIQUOTEWORKER_HPP)
#define QUASIQUOTEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class QuasiQuoteWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        QuasiQuoteWorker(ExprHandle form, Continuation * cont, Env * env);

        static QuasiQuoteWorker *
        makeInstance(ExprHandle form, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprHandle form;
        Continuation * cont;
        Env *       env;

        bool verify_single_form(ExprHandle input, Continuation * cont);
        void unquote_form(ExprHandle input, Continuation * cont);
        void splice_form(ExprHandle input, Continuation * cont);
        void cons_qq_list(ExprHandle car, ExprHandle cdr, Continuation * cont);
        void build_qq_list(ExprHandle input, Continuation * cont);
        void build_qq_form(ExprHandle input, Continuation * cont);
    };
}

#endif
