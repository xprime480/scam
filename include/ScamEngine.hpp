#if ! defined(SCAMENGINE_H)
#define SCAMENGINE_H 1

#include "Backtracker.hpp"
#include "Continuation.hpp"
#include "EngineMarker.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "input/ScamParser.hpp"

#include <set>
#include <vector>

namespace scam
{
    class Tokenizer;

    class ScamEngine
    {
    public:
        ScamEngine();
        ~ScamEngine();

        /***  functions to manage the frame stack ***/

        void reset(bool initEnv);

        void pushFrame();
        Env * getFrame();
        void popFrame();

        void addBinding(ScamExpr * key, ScamExpr * val);
        bool hasBinding(ScamExpr * key, bool checkParent = true);
        ScamExpr * getBinding(ScamExpr * key, bool top = false);
        void rebind(ScamExpr * key, ScamExpr * val);

        /*** functions to manage the input buffer ***/

        void pushInput(Tokenizer & tokenizer);
        void popInput();

        /*** functions to manage the output continuation ***/

        void setCont(Continuation * c);

        /*** functions to read, eval, and apply ***/

        ScamExpr * parseCurrentInput();
        ScamExpr * read();
        ScamExpr * eval(ScamExpr * expr);
        ScamExpr * apply(ScamExpr * expr, ScamExpr * args);

        /*** functions to manage backtracking */

        Backtracker * getBacktracker();
        void setBacktracker(Backtracker * backtracker);

        /*** functions to manage loading */

        bool isLoaded(std::string const & filename) const;
        void setLoaded(std::string const & filename);

        /*** function to manage garbage collection ***/

        void mark() const;

    private:
        Env * env;
        std::vector<ScamParser> input;
        Backtracker * backtracker;
        Continuation * cont;
        std::set<std::string> loaded;
	EngineMarker marker;

        void getStandardEnv();
    };
}

#endif

