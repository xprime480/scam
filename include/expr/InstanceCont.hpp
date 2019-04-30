#if ! defined(INSTANCECONT_HPP)
#define INSTANCECONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class InstanceCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        InstanceCont(ExprHandle obj,
                     ScamEnvKeyType name,
                     Continuation * cont);

        static InstanceCont * makeInstance(ExprHandle obj,
                                           ScamEnvKeyType name,
                                           Continuation * cont);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        ExprHandle obj;
        ScamEnvKeyType name;
        Continuation * cont;

        ExprHandle find_func(ExprHandle o) const;
        ExprHandle function_not_found() const;
    };
}

#endif
