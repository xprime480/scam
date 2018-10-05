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
                   std::shared_ptr<Continuation> cont,
                   Env & env) override;

        std::shared_ptr<ScamExpr> clone() override;
    };
}

#endif
