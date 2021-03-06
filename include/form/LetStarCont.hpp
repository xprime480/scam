#if ! defined(LETSTARCONT_HPP)
#define LETSTARCONT_HPP 1

#include "LetCommonCont.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetStarCont : public LetCommonCont
    {
    private:
        friend class scam::MemoryManager;

        LetStarCont(ScamValue formals,
                    ScamValue rest,
                    ScamValue forms,
                    Continuation * cont,
                    Env * env);

        static LetStarCont * makeInstance(ScamValue formals,
                                          ScamValue rest,
                                          ScamValue forms,
                                          Continuation * cont,
                                          Env * env);

    public:
        void mark() override;

    protected:
        ScamValue do_let(ScamValue expr) override;

    private:
        ScamValue formals;
        ScamValue rest;
        Env     * env;

        void makeBacktracker(ScamValue sym) const;
    };
}

#endif
