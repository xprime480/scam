#if ! defined(SCAMERROR_H)
#define SCAMERROR_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class ScamError : public ScamExpr
    {
    private:
        ScamError(char const * msg);

    public:
        static ScamError * makeInstance(char const * msg);

        std::string toString() const override;

        bool isNull() const override;
        bool error() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        std::string const msg;
    };
}

#endif
