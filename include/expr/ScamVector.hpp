#if ! defined(SCAMVECTOR_H)
#define SCAMVECTOR_H 1

#include "expr/ScamExpr.hpp"

#include <vector>

namespace scam
{
    class ScamVector : public ScamExpr
    {
    public:
        ScamVector(std::vector<std::shared_ptr<ScamExpr>> elts);

        std::string toString() const override;

        void eval(ScamContext & context);

        bool isVector() const override;

        size_t length() const override;
        std::shared_ptr<ScamExpr> nth(size_t n) const override;

        std::shared_ptr<ScamExpr> clone() override;

    private:
        std::vector<std::shared_ptr<ScamExpr>> elts;
    };
}

#endif
