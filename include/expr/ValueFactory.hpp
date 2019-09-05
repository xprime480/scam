#if ! defined(VALUEFACTORY_HPP)
#define VALUEFACTORY_HPP 1

#include "ScamFwd.hpp"
#include "expr/ScamData.hpp"
#include "expr/DictOps.hpp"
#include "util/MemoryManager.hpp"

#include <sstream>
#include <string>
#include <vector>

namespace scam
{
    class ClassDef;
    class LambdaDef;
    class SyntaxRules;

    using ExprVec = std::vector<ScamValue>;
    using ByteVec = std::vector<unsigned char>;

    extern void initializeValueFactory(MemoryManager & mm);

    extern ScamValue makeNothing();
    extern ScamValue makeNull();
    extern ScamValue makeBoolean(bool value);
    extern ScamValue makeCharacter(const char c);
    extern ScamValue makeString(std::string const & value, bool managed = true);

    extern ScamValue makeError(char const * msg, ExprVec & irritants);

    template <typename Irr, typename... Irrs>
    extern ScamValue makeError(char const * msg,
                               ExprVec irritants,
                               Irr && first,
                               Irrs... rest)
    {
        irritants.push_back(first);
        return makeError(msg, irritants, rest...);
    }

    template <typename... Irrs>
    extern ScamValue makeError(char const * msg, Irrs... irrs)
    {
        ExprVec irritants;
        return makeError(msg, irritants, irrs...);
    }

    extern ScamValue makeSymbol(std::string const & value);

    extern ScamValue makeKeyword(std::string const & value,
                                 bool managed = true);

    extern ScamValue makeNumeric(std::string const & value);
    extern ScamValue makeNaN();
    extern ScamValue makePosInf();
    extern ScamValue makeNegInf();
    extern ScamValue makeComplex(ScamValue real, ScamValue imag);
    extern ScamValue makeReal(double value, bool exact);
    extern ScamValue makeRational(int num, int den, bool exact);
    extern ScamValue makeInteger(int value, bool exact);

    extern ScamValue makePair(ScamValue car, ScamValue cdr);
    extern ScamValue makeList();
    extern ScamValue makeList(ScamValue item);
    extern ScamValue makeList(const std::vector<ScamValue> & items);

    template <typename... Args>
    ScamValue makeList(ScamValue car, Args... args)
    {
        return makePair(car, makeList(args...));
    }

    extern ScamValue makeVector(ExprVec const & elts);
    extern ScamValue makeByteVector(ByteVec const & elts);
    extern ScamValue makeDict();
    extern ScamValue makeDict(ExprVec const & args);

    extern ScamValue makeClosure(const LambdaDef & lambda, Env * env);
    extern ScamValue makeClass(ClassDef & def, Env * env);
    extern ScamValue makeClassInstance(ScamValue value, Env * env);

    extern ScamValue makeContinuation(Continuation * cont);

    extern ScamValue makeSpecialForm(std::string const & name,
                                     SfFunction func,
                                     bool managed = true);

    extern ScamValue makePrimitive(std::string const & name,
                                   PrimFunction func,
                                   bool managed = true);

    extern ScamValue makePort(ScamPort * port);
    extern ScamValue makeEof();

    extern ScamValue makeSyntax(const SyntaxRules & def);
    extern ScamValue makeEnv(Env * env);
    extern ScamValue makeForwarder(Env * env);
}

#endif
