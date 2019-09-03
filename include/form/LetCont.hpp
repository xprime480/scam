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

        LetCont(ScamValue formals,
                ScamValue forms,
                Continuation * cont,
                Env * env,
                bool rebind);


        static LetCont * makeInstance(ScamValue formals,
                                      ScamValue forms,
                                      Continuation * cont,
                                      Env * env,
                                      bool rebind);

    public:
        void mark() override;

    protected:
        ScamValue do_let(ScamValue expr) override;

    private:
        ScamValue formals;
        Env     * env;
        bool      rebind;

        ScamValue rebind_procs(Env * extended);
    };
}

#endif
