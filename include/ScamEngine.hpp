#if ! defined(SCAMENGINE_H)
#define SCAMENGINE_H 1

#include "Backtracker.hpp"
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
        bool hasBinding(ScamExpr * key, bool checkParent = true);
        ExprHandle getBinding(ScamExpr * key, bool top = false);
        void rebind(ScamExpr * key, ScamExpr * val);

        /*** functions to manage the input buffer ***/

        void pushInput(Tokenizer & tokenizer);
        void popInput();

        /*** functions to manage the output continuation ***/

        void setCont(ContHandle c);

        /*** functions to read, eval, and apply ***/

        ExprHandle parseCurrentInput();
        ExprHandle read();
        ExprHandle eval(ScamExpr * expr);
        ExprHandle apply(ScamExpr * expr, ScamExpr * args);

        /*** functions to manage backtracking */

        BacktrackHandle getBacktracker();
        void setBacktracker(BacktrackHandle backtracker);

    private:
        Env env;
        std::vector<ScamParser> input;
        BacktrackHandle backtracker;
        ContHandle cont;

        void getStandardEnv();
    };
}

#endif

