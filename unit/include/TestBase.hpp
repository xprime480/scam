#if !defined(TESTBASE_H)
#define TESTBASE_H 1

#include "Extractor.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamToInternal.hpp"

#include "TestHandler.hpp"

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
        TestHandler * handler;

        ScamValue evaluate(ScamValue input);
        ScamValue apply(ScamValue expr, ScamValue args);
        ScamValue readEval(std::string const & input);
        ScamValue readEvalFile(char const * filename);
        ScamValue readString(char const * input);

        void doCheck(bool act, unsigned selector, unsigned which);

        void checkPredicates(ScamValue expr, unsigned selector);

        void assertType(ScamValue value,
                        const char * name,
                        std::function<bool(ScamValue)> pred);

        void expectNothing(ScamValue expr);

        void expectError(ScamValue expr,
                         std::string const msg = "",
                         bool managed = true);

        void expectBoolean(ScamValue expr,
                           bool value,
                           std::string const & repr);

        void expectTrue(std::string const & input);
        void expectFalse(std::string const & input);

        void booleanTest(ScamValue expr,
                         bool value,
                         std::string const & repr);

        void expectSpecialNumeric(ScamValue expr, std::string const & repr);

        void expectComplex(ScamValue expr,
                           ScamValue real,
                           ScamValue imag,
                           std::string const & repr,
                           bool exact);

        void expectReal(ScamValue expr,
                        double value,
                        std::string const & repr,
                        bool exact);

        void expectRational(ScamValue expr,
                            const RationalPair & value,
                            std::string const & repr,
                            bool exact);

        void expectInteger(ScamValue expr,
                           int value,
                           std::string const & repr,
                           bool exact);

        void expectChar(ScamValue expr, char value, std::string const & repr);

        void expectString(ScamValue expr, std::string const & value);

        void expectSymbol(ScamValue expr, std::string const & name);

        void expectKeyword(ScamValue expr, std::string const & name);

        void expectNull(ScamValue expr);

        void expectList(ScamValue expr, std::string const & repr, size_t len);

        void expectPair(ScamValue expr, std::string const & repr);

        void expectApplicable(ScamValue expr,
                              std::string const & repr,
                              bool managed = false);

        void expectVector(ScamValue expr, std::string const & repr, size_t len);

        void expectByteVector(ScamValue expr,
                              std::string const & repr,
                              size_t len);

        void expectProcedure(ScamValue expr, std::string const & repr);

        void expectClass(ScamValue expr);

        void expectInstance(ScamValue expr);

        void expectDict(ScamValue expr, int count, std::string const & repr);

        void expectPort(ScamValue expr,
                        const std::string & repr,
                        const std::string & contents);
    };
}

#endif
