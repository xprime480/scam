#if ! defined(QUASIQUOTEWORKER_HPP)
#define QUASIQUOTEWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;

    class QuasiQuoteWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        QuasiQuoteWorker(ScamExpr * form, Continuation * cont, Env * env);

        static QuasiQuoteWorker *
        makeInstance(ScamExpr * form, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr * form;
        Continuation * cont;
        Env *       env;

        bool verify_single_form(ScamExpr * input, Continuation * cont);
        void unquote_form(ScamExpr * input, Continuation * cont);
        void splice_form(ScamExpr * input, Continuation * cont);
        void cons_qq_list(ScamExpr * car, ScamExpr * cdr, Continuation * cont);
        void build_qq_list(ScamExpr * input, Continuation * cont);
        void build_qq_form(ScamExpr * input, Continuation * cont);
    };
}

#endif
