#if ! defined(SCAMENGINE_H)
#define SCAMENGINE_H 1

#include "Backtracker.hpp"
#include "EngineMarker.hpp"
#include "Handler.hpp"
#include "ScamFwd.hpp"
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

        Env * getFrame();
        Env * getInteractionFrame();
        void setFrame(Env * env);

        /*** functions to manage the input buffer ***/

        void pushInput(Tokenizer & tokenizer);
        void popInput();

        /*** functions to manage the output continuation ***/

        void setCont(Continuation * c);

        /*** functions to read, eval, and apply ***/

        ScamValue readEvalCurrent();
        ScamValue read();
        ScamValue eval(ScamValue expr);
        ScamValue apply(ScamValue expr, ScamValue args);

        /*** functions to manage backtracking */

        Backtracker * getBacktracker();
        void setBacktracker(Backtracker * backtracker);

        /*** functions to manage loading */

        bool isLoaded(std::string const & filename) const;
        void setLoaded(std::string const & filename);

        /*** functions to manager error handling ***/

        void pushHandler(Handler * handler);
        ScamValue handleError(ScamValue err);
        void popHandler();

        /*** function to manage garbage collection ***/

        void mark();

    private:
        Env * env;
        Env * topEnv;
        std::vector<ScamParser> input;
        Backtracker * backtracker;
        Continuation * cont;
        std::set<std::string> loaded;
        std::vector<Handler *> handlers;
        EngineMarker marker;
    };
}

#endif

