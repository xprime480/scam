#if ! defined(ENV_H)
#define ENV_H 1

#include <memory>

namespace scam
{
    class ScamExpr;
    using ExprHandle = std::shared_ptr<ScamExpr>;

    struct EnvData;

    class Env
    {
    public:
        Env();

        void put(ScamExpr const * key, ScamExpr * val);
        bool check(ScamExpr const * key) const;
        ExprHandle get(ScamExpr const * key) const;

        Env extend() const;
        Env parent() const;
        Env top() const;

        void assign(ScamExpr const * key, ScamExpr * val);

        void dump(size_t max, bool full = false) const;

    private:
        std::shared_ptr<EnvData> data;
    };
}

#endif
