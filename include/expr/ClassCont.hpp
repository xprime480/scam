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
        void run(ScamValue expr) override;

    private:
        ClassHandle    cls;
        Continuation * cont;
        Env          * env;

        ScamValue build(ClassHandle cls, InstanceVec & instances) const;
        ScamInstance * connect(InstanceVec & instances) const;

        ScamValue get_parent(ScamClassAdapter const & adapter) const;

        ScamValue base_class_not_found(ScamEnvKeyType name) const;

        ScamValue
        base_class_not_class(ScamEnvKeyType name, ScamValue value) const;

        void init(ScamInstance * instance, ScamValue expr) const;
    };
}

#endif
