
#include "Extractor.hpp"
#include "ScamEngine.hpp"

#include <memory>
#include <string>

#include "gtest/gtest.h"

namespace scam
{
    class ScamParser;
    class Tokenizer;

    class ExpressionTestBase : public ::testing::Test
    {
    protected:
        ExpressionTestBase();
        virtual ~ExpressionTestBase();

        void SetUp() override;
        void TearDown() override;

        std::shared_ptr<Extractor> extractor;
        ScamEngine engine;
        MemoryManager & mm;

        ScamExpr * evaluate(ScamExpr * input);
        ScamExpr * apply(ScamExpr * expr, ScamExpr * args);
        ScamExpr * parseAndEvaluate(std::string const & input);
        ScamExpr * parseAndEvaluateFile(char const * filename);

        void doCheck(bool act, unsigned selector, unsigned which);

        void checkPredicates(ScamExpr * expr, unsigned selector);

        void expectNull(ScamExpr * expr);
        void expectError(ScamExpr * expr,
                         std::string const msg = "",
                         bool managed = true);

        void expectBoolean(ScamExpr * expr,
                           bool value,
                           std::string const & repr);
        void expectTrue(std::string const & input);
        void expectFalse(std::string const & input);
        void booleanTest(ScamExpr * expr,
                         bool value,
                         std::string const & repr);

        void expectFloat(ScamExpr * expr,
                         double value,
                         std::string const & repr);
        void expectInteger(ScamExpr * expr,
                           int value,
                           std::string const & repr);
        void expectChar(ScamExpr * expr,
                        char value,
                        std::string const & repr);
        void expectString(ScamExpr * expr,
                          std::string const & value);
        void expectSymbol(ScamExpr * expr,
                          std::string const & name);
        void expectKeyword(ScamExpr * expr,
                           std::string const & name);
        void expectNil(ScamExpr * expr);
        void expectList(ScamExpr * expr,
                        std::string const & repr,
                        size_t len);
        void expectCons(ScamExpr * expr,
                        std::string const & repr);
        void expectApplicable(ScamExpr * expr,
                              std::string const & repr,
                              bool managed = true);
        void expectVector(ScamExpr * expr,
                          std::string const & repr,
                          size_t len);
        void expectProcedure(ScamExpr * expr,
                             std::string const & repr);
        void expectClass(ScamExpr * expr);
        void expectInstance(ScamExpr * expr);
        void expectDict(ScamExpr * expr, int count, std::string const & repr);
    };
}
