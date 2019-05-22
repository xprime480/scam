#if ! defined(SCAMDICT_H)
#define SCAMDICT_H 1

#include "expr/ScamExpr.hpp"

#include <vector>

namespace scam
{
    using KeyVec = std::vector<ExprHandle>;
    using ValVec = std::vector<ExprHandle>;

    class ScamDict : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamDict();
        ScamDict(ValVec const & args);
        static ScamDict * makeInstance();
        static ScamDict * makeInstance(ValVec const & args);

    public:
        void
        apply(ExprHandle args, Continuation * cont, Env * env) override;

        size_t length() const override;

        bool equals(ConstExprHandle expr) const override;

        bool has(ExprHandle key) const;
        ExprHandle get(ExprHandle key) const;
        ExprHandle put(ExprHandle key, ExprHandle val);
        ExprHandle remove(ExprHandle key);
        KeyVec const & getKeys() const;
    };
}

#endif
