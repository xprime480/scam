#if ! defined(SPECIALFORM_H)
#define SPECIALFORM_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class SpecialForm : public ScamExpr
    {
    public:
        SpecialForm(std::string const & name, bool managed = true);

        std::string toString() const override;

        bool hasApply() const override;

    private:
        std::string const name;
    };
}

#endif
