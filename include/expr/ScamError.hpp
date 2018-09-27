#if ! defined(SCAMERROR_H)
#define SCAMERROR_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class ScamError : public ScamExpr
    {
    public:
        ScamError(char const * msg);
        std::string toString() const override;
        void eval(ScamContext & context) override;
        bool error() const override;

    private:
        std::string const msg;
    };
}

#endif
