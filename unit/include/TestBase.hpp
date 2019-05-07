#if !defined(TESTBASE_H)
#define TESTBASE_H 1

#include "Extractor.hpp"
#include "ScamEngine.hpp"

#include <memory>
#include <string>

#include "gtest/gtest.h"

namespace scam
{
    class ScamParser;
    class Tokenizer;

    class TestBase : public ::testing::Test
    {
    protected:
        TestBase();
        virtual ~TestBase();

        void SetUp() override;
        void TearDown() override;

        Extractor * extractor;
        ScamEngine engine;
        MemoryManager & mm;

        ExprHandle evaluate(ExprHandle input);
        ExprHandle apply(ExprHandle expr, ExprHandle args);
        ExprHandle parseAndEvaluate(std::string const & input);
        ExprHandle parseAndEvaluateFile(char const * filename);
        ExprHandle readString(char const * input);

        void doCheck(bool act, unsigned selector, unsigned which);

        void checkPredicates(ConstExprHandle expr, unsigned selector);

        void expectNull(ConstExprHandle expr);

        void expectError(ConstExprHandle expr,
                         std::string const msg = "",
                         bool managed = true);

        void expectBoolean(ConstExprHandle expr,
                           bool value,
                           std::string const & repr);

        void expectTrue(std::string const & input);
        void expectFalse(std::string const & input);

        void booleanTest(ConstExprHandle expr,
                         bool value,
                         std::string const & repr);

        void expectReal(ConstExprHandle expr,
                        double value,
                        std::string const & repr);

        void expectInteger(ConstExprHandle expr,
                           int value,
                           std::string const & repr);

        void expectChar(ConstExprHandle expr,
                        char value,
                        std::string const & repr);

        void expectString(ConstExprHandle expr,
                          std::string const & value);

        void expectSymbol(ConstExprHandle expr,
                          std::string const & name);

        void expectKeyword(ConstExprHandle expr,
                           std::string const & name);

        void expectNil(ConstExprHandle expr);

        void expectList(ConstExprHandle expr,
                        std::string const & repr,
                        size_t len);

        void expectCons(ConstExprHandle expr,
                        std::string const & repr);

        void expectApplicable(ConstExprHandle expr,
                              std::string const & repr);

        void expectVector(ConstExprHandle expr,
                          std::string const & repr,
                          size_t len);

        void expectProcedure(ConstExprHandle expr,
                             std::string const & repr);

        void expectClass(ConstExprHandle expr);

        void expectInstance(ConstExprHandle expr);

        void expectDict(ConstExprHandle expr,
                        int count,
                        std::string const & repr);
    };
}

#endif
