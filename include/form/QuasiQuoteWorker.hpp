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
        QuasiQuoteWorker(ScamValue form,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine);

        static QuasiQuoteWorker * makeInstance(ScamValue form,
                                               Continuation * cont,
                                               Env * env,
                                               ScamEngine * engine);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamValue form;
        Continuation * cont;
        Env *       env;

        bool verify_single_form(ScamValue input, Continuation * cont);
        void unquote_form(ScamValue input, Continuation * cont);
        void splice_form(ScamValue input, Continuation * cont);
        void cons_qq_list(ScamValue car, ScamValue cdr, Continuation * cont);
        void build_qq_list(ScamValue input, Continuation * cont);
        void build_qq_form(ScamValue input, Continuation * cont);
    };
}

#endif
