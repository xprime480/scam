#if ! defined(SCAMUNDEFINE_HPP)
#define SCAMUNDEFINE_HPP 1

#include "form/SpecialForm.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class Undefine : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;
        Undefine();
        static Undefine * makeInstance();

    public:
        void apply(ExprHandle args, Continuation * cont, Env * env) override;
    };
}

#endif
