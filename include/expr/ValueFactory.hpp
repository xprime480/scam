#if ! defined(VALUEFACTORY_HPP)
#define VALUEFACTORY_HPP 1

#include "expr/ScamData.hpp"
#include "ScamFwd.hpp"
#include "expr/DictOps.hpp"
#include "util/MemoryManager.hpp"

#include <sstream>
#include <string>
#include <vector>

namespace scam
{
    class ClassDefParser;
    class LambdaParser;

    using ExprVec = std::vector<ScamValue>;
    using ByteVec = std::vector<unsigned char>;

    extern ScamValue makeNothing();
    extern ScamValue makeNull();
    extern ScamValue makeBoolean(bool value);
    extern ScamValue makeCharacter(const char c);
    extern ScamValue makeString(std::string const & value);

    extern ScamValue makeError(char const * msg, bool managed = true);
    extern ScamValue makeError(std::string const & msg, bool managed = true);

    template <typename ... Ts>
    ScamValue makeErrorExtended(Ts && ... args)
    {
        std::stringstream s;
        bool iHateTheCompiler { true };

        if ( sizeof...(Ts) > 0 ) {
            int dummy[sizeof...(Ts)] = { (s << args, 0)... };
            iHateTheCompiler = dummy[0] == 0;
        }

        const std::string msg = (iHateTheCompiler
                                 ? s.str()
                                 : std::string("never"));
        return makeError(msg, true);
    }

    extern ScamValue makeSymbol(std::string const & value,
                                bool managed = true);

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
    extern ScamValue makeList(std::vector<ScamValue> & items);

    template <typename... Args>
    ScamValue makeList(ScamValue car, Args... args)
    {
        return makePair(car, makeList(args...));
    }

    extern ScamValue makeVector(ExprVec const & elts);
    extern ScamValue makeByteVector(ByteVec const & elts);
    extern ScamValue makeDict();
    extern ScamValue makeDict(ExprVec const & args);

    extern ScamValue makeClosure(LambdaParser * parser,
                                 Env * env,
                                 bool macrolike = false);

    extern ScamValue makeClass(ClassDefParser * def, Env * env);
    extern ScamValue makeClassInstance(ScamValue value, Env * env);

    extern ScamValue makeContinuation(Continuation * cont);

    extern ScamValue makeSpecialForm(std::string const & name,
                                     SfFunction func,
                                     ScamEngine * engine = nullptr,
                                     bool managed = true);

    extern ScamValue makePrimitive(std::string const & name,
                                   PrimFunction func,
                                   ScamEngine * engine = nullptr,
                                   bool managed = true);

    extern ScamValue makePort(ScamPort * port);
}

#endif
