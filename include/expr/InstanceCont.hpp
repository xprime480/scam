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

        InstanceCont(ScamValue obj, ScamValue name, Continuation * cont);

        static InstanceCont *
        makeInstance(ScamValue obj, ScamValue name, Continuation * cont);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        ScamValue obj;
        ScamValue name;
        Continuation * cont;

        ScamValue find_func(ScamValue o) const;
        ScamValue function_not_found() const;
    };
}

#endif
