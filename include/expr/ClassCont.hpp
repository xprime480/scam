#if ! defined(CLASSCONT_HPP)
#define CLASSCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

#include <vector>

namespace scam
{

    class ClassCont : public Continuation
    {
    private:
        using InstanceVec = std::vector<ScamValue>;

        friend class scam::MemoryManager;
        ClassCont(ScamValue cls, Continuation * cont, ScamEngine * engine);

        static ClassCont *
        makeInstance(ScamValue cls, Continuation * cont, ScamEngine * engine);

    public:
        void mark() const override;
        void handleValue(ScamValue expr) override;

    private:
        ScamValue    cls;
        Continuation * cont;
        Env          * env;

        ScamValue build(ScamValue cls, InstanceVec & instances) const;
        ScamValue connect(InstanceVec & instances) const;
        ScamValue get_parent(ScamValue value) const;
        ScamValue base_class_not_found(ScamValue name) const;
        ScamValue base_class_not_class(ScamValue name, ScamValue value) const;
        void init(ScamValue instance, ScamValue expr) const;
    };
}

#endif
