#if ! defined(SCAMCONS_H)
#define SCAMCONS_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCons : public ScamExpr
    {
    public:
        ScamCons(std::shared_ptr<ScamExpr> const & car,
                 std::shared_ptr<ScamExpr> const & cdr);

        std::string toString() const override;
        void eval(std::shared_ptr<Continuation> cont, Env & env) override;

        bool isCons() const override;
        bool isList() const override;

        std::shared_ptr<ScamExpr> getCar() const override;
        std::shared_ptr<ScamExpr> getCdr() const override;

        size_t length() const override;
        std::shared_ptr<ScamExpr> nth(size_t n) const override;

        std::shared_ptr<ScamExpr> clone() override;

    private:
        std::shared_ptr<ScamExpr> car;
        std::shared_ptr<ScamExpr> cdr;
    };
}

#endif
