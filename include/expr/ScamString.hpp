#if ! defined(SCAMSTRING_H)
#define SCAMSTRING_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamString : public ScamExpr
    {
    public:
        ScamString(std::string const & value);
        std::string toString() const override;

        bool isString() const override;

        std::shared_ptr<ScamExpr> clone() override;

    private:
        std::string const value;
    };
}

#endif
