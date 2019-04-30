#if ! defined(CLASSCONT_HPP)
#define CLASSCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"
#include "expr/ExprFwd.hpp"
#include "expr/ScamClassAdapter.hpp"

#include <vector>

namespace scam
{

    class ClassCont : public Continuation
    {
    private:
        using InstanceVec = std::vector<ScamInstance *>;
        using ClassHandle = const ScamClass *;

        friend class scam::MemoryManager;
        ClassCont(ClassHandle cls, Continuation * cont);
        static ClassCont * makeInstance(ClassHandle cls, Continuation * cont);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        ClassHandle    cls;
        Continuation * cont;
        Env          * env;

        ExprHandle build(ClassHandle cls, InstanceVec & instances) const;
        ScamInstance * connect(InstanceVec & instances) const;

        ExprHandle get_parent(ScamClassAdapter const & adapter) const;

        ExprHandle base_class_not_found(ScamEnvKeyType name) const;

        ExprHandle
        base_class_not_class(ScamEnvKeyType name, ExprHandle value) const;

        void init(ScamInstance * instance, ExprHandle expr) const;
    };
}

#endif
