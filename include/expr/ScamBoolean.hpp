#if ! defined(SCAMBOOLEAN_H)
#define SCAMBOOLEAN_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamBoolean : public ScamExpr
    {
    public:
        ScamBoolean(bool value);
        std::string toString() const override;

        void eval(ScamContext & context) override;

        bool isNull() const override;

    private:
        bool value;
    };
}

#endif
