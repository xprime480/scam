#if ! defined(SCAMNULL_H)
#define SCAMNULL_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class ScamContext;

    class ScamNull : public ScamExpr
    {
    public:
        std::string toString() const override;
        void eval(ScamContext & context) override;

        bool isNull() const override;
        bool truth() const override;

	std::shared_ptr<ScamExpr> clone() override;
    };
}

#endif
