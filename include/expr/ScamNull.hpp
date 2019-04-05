#if ! defined(SCAMNULL_H)
#define SCAMNULL_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{

  class ScamNull : public ScamExpr
    {
    private:
        ScamNull();

    public:
        static ScamNull * makeInstance();
        bool isManaged() const override;

        std::string toString() const override;
        void eval(ContHandle cont, Env env) override;

        bool isNull() const override;
        bool truth() const override;

        bool equals(ScamExpr const * expr) const override;
    };
}

#endif
