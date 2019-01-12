#if ! defined(SCAMDICT_H)
#define SCAMDICT_H 1

#include "expr/ScamExpr.hpp"

#include <vector>

namespace scam
{
    using ExprVec = std::vector<ExprHandle>;

    class ScamDict : public ScamExpr
    {
    public:
        ScamDict();
        ScamDict(ExprVec const & args);

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ScamExpr * args, ContHandle cont, Env env) override;

        bool truth() const override;

        bool isDict() const override;

        size_t length() const override;

        bool equals(ScamExpr const * expr) const override;

        ExprHandle get(ScamExpr const * key) const;
        ExprHandle put(ScamExpr const * key, ScamExpr const * val);
        ExprVec const & getKeys() const;

    private:
        ExprVec keys;
        ExprVec vals;
    };
}

#endif
