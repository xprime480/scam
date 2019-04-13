#if ! defined(INSTANCECONT_HPP)
#define INSTANCECONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class Env;
    class ScamExpr;
    class MemoryManager;

    class InstanceCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        InstanceCont(ScamExpr * obj, ScamExpr * name, Continuation * cont);

        static InstanceCont *
        makeInstance(ScamExpr * obj, ScamExpr * name, Continuation * cont);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        ScamExpr * obj;
        ScamExpr * name;
        Continuation * cont;

        ScamExpr * find_func(ScamExpr * o) const;
        ScamExpr * function_not_found() const;
    };
}

#endif
