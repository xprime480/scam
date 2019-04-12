#if ! defined(LETCONT_HPP)
#define LETCONT_HPP 1

#include "LetCommonCont.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;

    class LetCont : public LetCommonCont
    {
    private:
        friend class scam::MemoryManager;

        LetCont(ScamExpr * formals,
                ScamExpr * forms,
                Continuation * cont,
                Env * env,
                bool rebind);


        static LetCont * makeInstance(ScamExpr * formals,
                                      ScamExpr * forms,
                                      Continuation * cont,
                                      Env * env,
                                      bool rebind);

    public:
        void mark() const override;

    protected:
        void do_let(ScamExpr * expr) override;

    private:
        ScamExpr * formals;
        Env *        env;
        bool       rebind;

        void rebind_procs(Env * extended);
    };
}

#endif
