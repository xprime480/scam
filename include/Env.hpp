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
        bool check(ScamExpr const * key, bool checkParent = true) const;
        ExprHandle get(ScamExpr const * key) const;

        void reset();
        Env extend() const;
        Env parent() const;
        Env top() const;

        void assign(ScamExpr const * key, ScamExpr * val);
        void remove(ScamExpr const * key);

        void dump(size_t max, bool full = false) const;

    private:
        std::shared_ptr<EnvData> data;
    };
}

#endif
