#if ! defined(SCAMENGINE_H)
#define SCAMENGINE_H 1

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "input/ScamParser.hpp"

#include <vector>

namespace scam
{
    class Tokenizer;

    class ScamEngine
    {
    public:
        ScamEngine();

        /***  functions to manage the frame stack ***/

        void reset(bool initEnv);

        void pushFrame();
        Env getFrame();
        void popFrame();

        void addBinding(ScamExpr * key, ScamExpr * val);
        bool hasBinding(ScamExpr * key);
        ExprHandle getBinding(ScamExpr * key, bool top = false);
        void rebind(ScamExpr * key, ScamExpr * val);

        /*** functions to manage the input buffer ***/

        void pushInput(Tokenizer & tokenizer);
        void popInput();

        /*** functions to read, eval, and apply ***/

        ExprHandle read();
        void eval(ScamExpr * expr, ContHandle cont);
        void apply(ScamExpr * expr, ScamExpr * args, ContHandle cont);

    private:
        Env env;
        std::vector<ScamParser> input;

        void getStandardEnv();
    };
}

#endif

