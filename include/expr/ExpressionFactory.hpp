#if ! defined(EXPRESSIONFACTORY_H)
#define EXPRESSIONFACTORY_H 1

#include "expr/ScamExprAll.hpp"
#include "input/LambdaParser.hpp"
#include "util/MemoryManager.hpp"

#include <string>
#include <vector>
#include <sstream>

namespace scam
{
    class Continuation;
    class ClassDefParser;
    class ScamClass;

    using ExprVec = std::vector<ExprHandle>;

    class ExpressionFactory
    {
    public:
        static ScamNull * makeNull();

        static ScamError * makeError(char const * msg, bool managed = true);
        static ScamError * makeError(std::string const & msg,
                                     bool managed = true);

        template <typename ... Ts>
        static ScamError * makeError(Ts && ... args)
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

        static ScamBoolean * makeBoolean(bool value);
        static ScamCharacter * makeCharacter(std::string const & value);

        static ScamString * makeString(std::string const & value);
        static ScamSymbol * makeSymbol(std::string const & value,
                                       bool managed = true);
        static ScamKeyword * makeKeyword(std::string const & value,
                                         bool managed = true);

        static ExprHandle makeNumeric(std::string const & value);

        static ScamNaN * makeNaN();
        static ScamPosInf * makePosInf();
        static ScamNegInf * makeNegInf();

        static ScamReal * makeReal(double value, bool exact);
        static ScamRational * makeRational(int num, int den, bool exact);
        static ScamNumeric * makeInteger(int value, bool exact);

        static ScamNil * makeNil();
        static ScamCons * makeCons(ExprHandle car, ExprHandle cdr);

        static ExprHandle makeList();
        static ExprHandle makeList(ExprHandle item);
        static ExprHandle makeList(std::vector<ExprHandle> & items);

        template <typename... Args>
        static ExprHandle makeList(ExprHandle car, Args... args)
        {
            return makeCons(car, makeList(args...));
        }

        static ScamVector * makeVector(ExprVec const & elts);

        static ScamClosure * makeClosure(const LambdaParser * parser,
                                         Env * env,
                                         bool macrolike = false);

        static ScamClass * makeClass(ClassDefParser * def, Env * env);
        static ScamInstance * makeInstance(const ScamClass * cls, Env * env);

        static ScamContinuation * makeContinuation(Continuation * cont);

        static ScamDict * makeDict();
        static ScamDict * makeDict(ExprVec const & args);

        template <typename T, typename... Args>
        static T * makeForm(Args... args)
        {
            return standardMemoryManager.make<T>(args...);
        }
    };
}
#endif
