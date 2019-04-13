#if ! defined(SCAMDICT_H)
#define SCAMDICT_H 1

#include "expr/ScamExpr.hpp"

#include <vector>

namespace scam
{
    using KeyVec = std::vector<ScamExpr const *>;
    using ValVec = std::vector<ScamExpr *>;

    class ScamDict : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamDict();
        ScamDict(ValVec const & args);
        static ScamDict * makeInstance();
        static ScamDict * makeInstance(ValVec const & args);

    public:
        void mark() const override;

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;

        bool truth() const override;

        bool isDict() const override;

        size_t length() const override;

        bool equals(ScamExpr const * expr) const override;

        bool has(ScamExpr const * key) const;
        ScamExpr * get(ScamExpr const * key) const;
        ScamExpr * put(ScamExpr const * key, ScamExpr * val);
        ScamExpr * remove(ScamExpr const * key);
        KeyVec const & getKeys() const;

    private:
        KeyVec keys;
        ValVec vals;

        void bad_op(ScamExpr * op, Continuation * cont);
        void exec_get(ScamExpr * args, Continuation * cont);
        void exec_put(ScamExpr * args, Continuation * cont);
        void exec_has(ScamExpr * args, Continuation * cont);
        void exec_remove(ScamExpr * args, Continuation * cont);
        void exec_length(Continuation * cont);
        void exec(ScamExpr * args, Continuation * cont);
    };
}

#endif
