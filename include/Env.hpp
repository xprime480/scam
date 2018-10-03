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

        void put(std::shared_ptr<ScamExpr> key, std::shared_ptr<ScamExpr> val);
	bool check(std::shared_ptr<ScamExpr> key) const;
        std::shared_ptr<ScamExpr> get(std::shared_ptr<ScamExpr> key) const;

        Env extend();

        void
        assign(std::shared_ptr<ScamExpr> key, std::shared_ptr<ScamExpr> val);

        void dump(size_t max) const;

    private:
        std::shared_ptr<EnvData> data;
    };
}

#endif
