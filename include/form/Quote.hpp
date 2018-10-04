#if ! defined(SCAMQUOTE_H)
#define SCAMQUOTE_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Quote : public SpecialForm
    {
    public:
        Quote();

        void apply(std::shared_ptr<ScamExpr> const & args,
                   ScamContext const & context) override;

        std::shared_ptr<ScamExpr> clone() override;
    };
}

#endif
