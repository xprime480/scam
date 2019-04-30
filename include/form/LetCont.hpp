#if ! defined(LETCONT_HPP)
#define LETCONT_HPP 1

#include "LetCommonCont.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    

    class LetCont : public LetCommonCont
    {
    private:
        friend class scam::MemoryManager;

        LetCont(ExprHandle formals,
                ExprHandle forms,
                Continuation * cont,
                Env * env,
                bool rebind);


        static LetCont * makeInstance(ExprHandle formals,
                                      ExprHandle forms,
                                      Continuation * cont,
                                      Env * env,
                                      bool rebind);

    public:
        void mark() const override;

    protected:
        void do_let(ExprHandle expr) override;

    private:
        ExprHandle formals;
        Env *        env;
        bool       rebind;

        void rebind_procs(Env * extended);
    };
}

#endif
