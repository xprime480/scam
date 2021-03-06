#if ! defined(SCAMENGINE_H)
#define SCAMENGINE_H 1

#include "Backtracker.hpp"
#include "EngineMarker.hpp"
#include "Handler.hpp"
#include "ScamFwd.hpp"
#include "input/ScamParser.hpp"
#include "util/MemoryManager.hpp"

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

        static ScamEngine & getEngine();

        /***  functions to manage the frame stack ***/

        void release();
        void reset(bool initEnv);

        Env * getFrame();
        Env * getConfigFrame();
        Env * getSyntaxFrame();
        Env * getInteractionFrame();
        void setFrame(Env * env);

        /*** functions to manage libraries ***/

        ScamValue findLibrary(ScamValue name);
        void saveLibrary(ScamValue name, Env * env);

        /*** functions to manage the input buffer ***/

        void pushInput(Tokenizer & tokenizer);
        void popInput();

        /*** functions to manage the output continuation ***/

        void setCont(Continuation * c);

        /*** functions to read, eval, and apply ***/

        ScamValue readEvalCurrent();
        ScamValue read();
        ScamValue eval(ScamValue expr);
        ScamValue eval(ScamValue expr, Handler * handler);
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

        MemoryManager & getMemoryManager();
        void mark();

    private:
        Env * configEnv;
        Env * syntaxEnv;
        Env * env;
        Env * topEnv;
        ScamValue libs;
        std::vector<ScamParser> input;
        Backtracker * backtracker;
        Continuation * cont;
        std::set<std::string> loaded;
        std::vector<Handler *> handlers;
        EngineMarker marker;
        MemoryManager mm;
    };
}

#endif

