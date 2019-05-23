#if !defined(TESTBASE_H)
#define TESTBASE_H 1

#include "Extractor.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamToInternal.hpp"

#include <functional>
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
        TestBase(bool loadPrelude = true);
        virtual ~TestBase();

        void SetUp() override;
        void TearDown() override;

        Extractor * extractor;
        ScamEngine engine;
        MemoryManager & mm;

        ScamValue evaluate(ScamValue input);
        ScamValue apply(ScamValue expr, ScamValue args);
        ScamValue parseAndEvaluate(std::string const & input);
        ScamValue parseAndEvaluateFile(char const * filename);
        ScamValue readString(char const * input);

        void doCheck(bool act, unsigned selector, unsigned which);

        void checkPredicates(ConstScamValue expr, unsigned selector);
        void assertType(ConstScamValue value,
                        const char * name,
                        std::function<bool(ConstScamValue)> pred);

        void expectNull(ConstScamValue expr);

        void expectError(ConstScamValue expr,
                         std::string const msg = "",
                         bool managed = true);

        void expectBoolean(ConstScamValue expr,
                           bool value,
                           std::string const & repr);

        void expectTrue(std::string const & input);
        void expectFalse(std::string const & input);

        void booleanTest(ConstScamValue expr,
                         bool value,
                         std::string const & repr);

        void expectSpecialNumeric(ConstScamValue expr,
                                  std::string const & repr);

        void expectComplex(ConstScamValue expr,
                           ConstScamValue real,
                           ConstScamValue imag,
                           std::string const & repr,
                           bool exact);

        void expectReal(ConstScamValue expr,
                        double value,
                        std::string const & repr,
                        bool exact);

        void expectRational(ConstScamValue expr,
                            const RationalPair & value,
                            std::string const & repr,
                            bool exact);

        void expectInteger(ConstScamValue expr,
                           int value,
                           std::string const & repr,
                           bool exact);

        void expectChar(ConstScamValue expr,
                        char value,
                        std::string const & repr);

        void expectString(ConstScamValue expr,
                          std::string const & value);

        void expectSymbol(ConstScamValue expr,
                          std::string const & name);

        void expectKeyword(ConstScamValue expr,
                           std::string const & name);

        void expectNil(ConstScamValue expr);

        void expectList(ConstScamValue expr,
                        std::string const & repr,
                        size_t len);

        void expectCons(ConstScamValue expr,
                        std::string const & repr);

        void expectApplicable(ConstScamValue expr,
                              std::string const & repr);

        void expectVector(ConstScamValue expr,
                          std::string const & repr,
                          size_t len);

        void expectByteVector(ConstScamValue expr,
                              std::string const & repr,
                              size_t len);

        void expectProcedure(ConstScamValue expr,
                             std::string const & repr);

        void expectClass(ConstScamValue expr);

        void expectInstance(ConstScamValue expr);

        void expectDict(ConstScamValue expr,
                        int count,
                        std::string const & repr);
    };
}

#endif
