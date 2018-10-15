
#include "Env.hpp"
#include "Extractor.hpp"

#include <memory>
#include <string>

#include "gtest/gtest.h"

namespace scam
{
    class ExpressionTestBase : public ::testing::Test
    {
    protected:
        ExpressionTestBase();
        virtual ~ExpressionTestBase();

        void SetUp() override;
        void TearDown() override;

        std::shared_ptr<Extractor> extractor;
        Env env;

        ExprHandle evaluate(ExprHandle input);
        ExprHandle parseAndEvaluate(std::string const & input);

        void doCheck(bool act, unsigned selector, unsigned which);

        void checkPredicates(ExprHandle expr, unsigned selector);

        void expectNull(ExprHandle expr);
        void expectError(ExprHandle expr,
                         std::string const msg = "");

        void expectBoolean(ExprHandle expr,
                           bool value,
                           std::string const & repr);
        void booleanTest(ExprHandle expr,
                         bool value,
                         std::string const & repr);

        void expectFloat(ExprHandle expr,
                         double value,
                         std::string const & repr);
        void expectInteger(ExprHandle expr,
                           int value,
                           std::string const & repr);
        void expectChar(ExprHandle expr,
                        char value,
                        std::string const & repr);
        void expectString(ExprHandle expr,
                          std::string const & value);
        void expectSymbol(ExprHandle expr,
                          std::string const & name);
        void expectNil(ExprHandle expr);
        void expectList(ExprHandle expr,
                        std::string const & repr,
                        size_t len);
        void expectCons(ExprHandle expr,
                        std::string const & repr);
        void expectApplicable(ExprHandle expr,
                              std::string const & repr);
        void expectVector(ExprHandle expr,
                          std::string const & repr,
                          size_t len);
        void expectProcedure(ExprHandle expr,
                             std::string const & repr);
    };
}
