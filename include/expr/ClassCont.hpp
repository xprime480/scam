#if ! defined(CLASSCONT_HPP)
#define CLASSCONT_HPP 1

#include "Continuation.hpp"

#include "expr/ScamClassAdapter.hpp"

#include <vector>

namespace scam
{
    class Env;
    class ScamExpr;
    class MemoryManager;

    class ClassCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        ClassCont(ScamExpr * cls, Continuation * cont);
        static ClassCont * makeInstance(ScamExpr * cls, Continuation * cont);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        ScamExpr * cls;
        Continuation * cont;
        Env *        env;

        ScamExpr *
        build_instances(ScamExpr * cls,
                        std::vector<ScamExpr *> & instances) const;

        ScamExpr *
        connect_instances(std::vector<ScamExpr *> & instances) const;

        ScamExpr * get_parent(ScamClassAdapter const & adapter) const;
        ScamExpr * no_class_found(ScamExpr * cls) const;
        ScamExpr * base_class_not_found(ScamExpr * name) const;

        ScamExpr *
        base_class_not_class(ScamExpr * name, ScamExpr * value) const;

        void init_instance(ScamExpr * instance, ScamExpr * expr) const;
    };
}

#endif
