
#include "Env.hpp"
#include "Extractor.hpp"

#include <memory>

#include "gtest/gtest.h"

namespace scam
{
    class ExpressionTestBase : public ::testing::Test
    {
    protected:
        ExpressionTestBase();

        std::shared_ptr<Extractor> extractor;
        Env env;

        std::shared_ptr<ScamExpr> evaluate(std::shared_ptr<ScamExpr> input);

        void doCheck(bool act, unsigned selector, unsigned which);

        void checkPredicates(std::shared_ptr<ScamExpr> expr, unsigned selector);

        void expectNull(std::shared_ptr<ScamExpr> expr);
        void expectError(std::shared_ptr<ScamExpr> expr,
                         std::string const msg = "");

        void expectBoolean(std::shared_ptr<ScamExpr> expr,
                           bool value,
                           std::string const & repr);
        void booleanTest(std::shared_ptr<ScamExpr> expr,
                         bool value,
                         std::string const & repr);

        void expectFloat(std::shared_ptr<ScamExpr> expr,
                         double value,
                         std::string const & repr);
        void expectInteger(std::shared_ptr<ScamExpr> expr,
                           int value,
                           std::string const & repr);
        void expectChar(std::shared_ptr<ScamExpr> expr,
                        char value,
                        std::string const & repr);
        void expectString(std::shared_ptr<ScamExpr> expr,
                          std::string const & value);
        void expectSymbol(std::shared_ptr<ScamExpr> expr,
                          std::string const & name);
        void expectNil(std::shared_ptr<ScamExpr> expr,
                       std::string const & repr);
        void expectList(std::shared_ptr<ScamExpr> expr,
                        std::string const & repr,
                        size_t len);
        void expectCons(std::shared_ptr<ScamExpr> expr,
                        std::string const & repr);
        void expectApplicable(std::shared_ptr<ScamExpr> expr,
                              std::string const & repr);
        void expectVector(std::shared_ptr<ScamExpr> expr,
                          std::string const & repr,
                          size_t len);
    };
}
