#if ! defined(ENV_H)
#define ENV_H 1

#include "expr/ScamExpr.hpp"

#include <memory>

namespace scam
{
    struct EnvData;

    class Env
    {
    public:
        Env();

        void put(ExprHandle key, ExprHandle val);
        bool check(ExprHandle key) const;
        ExprHandle get(ExprHandle key) const;

        Env extend();

        void assign(ExprHandle key, ExprHandle val);

        void dump(size_t max) const;

    private:
        std::shared_ptr<EnvData> data;
    };
}

#endif
