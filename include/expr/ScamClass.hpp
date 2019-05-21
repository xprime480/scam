#if ! defined(SCAMCLASS_H)
#define SCAMCLASS_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ClassDefParser;
    class Env;
    class ScamClassAdapter;

    class ScamClass : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamClass(ClassDefParser * def, Env * capture);
        static ScamClass * makeInstance(ClassDefParser * def, Env * capture);

    public:
        void mark() const override;

        std::string toString() const override;

        bool hasApply() const override;

        void
        apply(ExprHandle args, Continuation * cont, Env * env) override;

        friend class ScamClassAdapter;

    private:
        ClassDefParser * def;
        Env            * capture;
    };
}

#endif
