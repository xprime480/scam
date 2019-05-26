#if ! defined(SCAMDICT_H)
#define SCAMDICT_H 1

#include "expr/ScamExpr.hpp"

#include <vector>

namespace scam
{
    using KeyVec = std::vector<ScamValue>;
    using ValVec = std::vector<ScamValue>;

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
        apply(ScamValue args, Continuation * cont, Env * env) override;

        bool has(ScamValue key) const;
        ScamValue get(ScamValue key) const;
        ScamValue put(ScamValue key, ScamValue val);
        ScamValue remove(ScamValue key);
        KeyVec const & getKeys() const;
    };
}

#endif
