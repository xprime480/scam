
#include "prim/SystemOps.hpp"

#include "Backtracker.hpp"
#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/EvalString.hpp"

#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void
    apply_load(ScamExpr * args, Continuation * cont, ScamEngine * engine);

    extern void apply_spawn(Continuation * cont);
    extern void apply_error(ScamExpr * args, Continuation * cont);
    extern void apply_backtrack(Continuation * cont, ScamEngine * engine);
    extern void apply_trace(ScamExpr * args, Continuation * cont);
}

Load::Load(ScamEngine * engine)
    : Primitive("load")
    , engine(engine)
{
}

Load * Load::makeInstance(ScamEngine * engine)
{
    return new Load(engine);
}

void Load::applyArgs(ScamExpr * args, Continuation * cont)
{
    apply_load(args, cont, engine);
}

Spawn::Spawn()
    : Primitive("spawn")
{
}

Spawn * Spawn::makeInstance()
{
    return new Spawn();
}

void Spawn::applyArgs(ScamExpr * args, Continuation * cont)
{
    apply_spawn(cont);
}

Error::Error()
    : Primitive("error")
{
}

Error * Error::makeInstance()
{
    return new Error();
}

void Error::applyArgs(ScamExpr * args, Continuation * cont)
{
    apply_error(args, cont);
}

Backtrack::Backtrack(ScamEngine * engine)
    : Primitive("backtrack")
    , engine(engine)
{
}

Backtrack * Backtrack::makeInstance(ScamEngine * engine)
{
    return new Backtrack(engine);
}

void Backtrack::applyArgs(ScamExpr * args, Continuation * cont)
{
    apply_backtrack(cont, engine);
}

Trace::Trace()
    : Primitive("trace")
{
}

Trace * Trace::makeInstance()
{
    return new Trace();
}

void Trace::applyArgs(ScamExpr * args, Continuation * cont)
{
    apply_trace(args, cont);
}

namespace
{
    extern ScamExpr * default_path();

    string next_element(char const *& path)
    {
        stringstream s;
        while ( true ) {
            const char c = *path++;
            if ( ':' == c || ! c  ) {
                return s.str();
            }
            s << c;
        }
    }

    ScamExpr * convert_path(char const * path)
    {
        ExprVec dp;

        while ( *path ) {
            string element = next_element(path);
            if ( ! element.empty() ) {
                dp.push_back(ExpressionFactory::makeString(element));
            }
        }

        if ( dp.empty() ) {
            return default_path();
        }
        return ExpressionFactory::makeVector(dp);
    }

    ScamExpr * default_path()
    {
        return convert_path(".:..");
    }

    ScamExpr * get_path()
    {
        ScamExpr * rv = ExpressionFactory::makeNull();

        char const * path = getenv("SCAM_PATH");
        if ( ! path || ! *path ) {
            rv = default_path();
        }
        else {
            rv = convert_path(path);
        }
        return rv;
    }

    string make_path(string dirname, string filename)
    {
        stringstream s;
        s << dirname << "/" << filename;
        return s.str();
    }

    bool file_exists(string fullpath)
    {
        ifstream x;
        x.open(fullpath);
        if ( x.good() ) {
            x.close();
            return true;
        }

        return false;
    }

    void file_not_found(string const & filename, Continuation * cont)
    {
        stringstream s;
        s << "Unable to open file " << filename;
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }

    bool open_file(ifstream & source,
                   string const & filename,
                   Continuation * cont)
    {
        if ( '/' == filename.at(0) ) {
            if ( file_exists(filename) ) {
                source.open(filename);
                return true;
            }
        }
        else {
            ScamExpr * path = get_path();

            size_t n = path->length();
            for ( size_t i = 0 ; i < n ; ++i ) {
                string fullpath = make_path(path->nthcar(i)->toString(), filename);
                if ( file_exists(fullpath) ) {
                    source.open(fullpath);
                    return true;
                }
            }
        }

        file_not_found(filename, cont);
        return false;
    }

    string get_data(ifstream & source)
    {
        char buf[1024];
        stringstream text;

        while ( source.good() && ! source.eof() ) {
            source.getline(buf, sizeof ( buf ));
            text << buf << "\n";
        }
        source.close();

        return text.str();
    }

    void apply_load(ScamExpr * args, Continuation * cont, ScamEngine * engine)
    {
        string filename = args->nthcar(0)->toString();
        if ( engine->isLoaded(filename) ) {
            stringstream s;
            s << "file \"" << filename << "\" already loaded";
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
            return;
        }

        ifstream source;

        if ( ! open_file(source, filename, cont) ) {
            return;
        }

        string data = get_data(source);
        EvalString helper(engine, data);
        ScamExpr * last = helper.run();

        engine->setLoaded(filename);
        cont->run(last);
    }

    class SpawnWorker : public Worker
    {
    public:
        SpawnWorker(Continuation * cont, bool value)
            : Worker("SpawnWorker")
            , cont(cont)
            , value(value)
        {
        }

        void run() override
        {
            ScamExpr * flag = ExpressionFactory::makeBoolean(value);
            cont->run(flag);
        }

    private:
        Continuation * cont;
        bool       value;
    };

    void apply_spawn(Continuation * cont)
    {
        bool t { true };
        workQueueHelper<SpawnWorker>(cont, t);
        bool f { false };
        workQueueHelper<SpawnWorker>(cont, f);
    }

    void apply_error(ScamExpr * args, Continuation * cont)
    {
        stringstream s;
        unsigned len = args->length();
        if ( 0 == len ) {
            s << "Error detected";
        }
        else if ( 1 == len ) {
            s << args->nthcar(0)->toString();
        }
        else {
            for ( unsigned i = 0 ; i < len ; ++i ) {
                s << "[" << (i+1) << "] "
                  << args->nthcar(i)->toString() << "\n";
            }
        }

        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }

    void apply_backtrack(Continuation * cont, ScamEngine * engine)
    {
        BacktrackHandle backtracker = engine->getBacktracker();
        if ( nullptr == backtracker.get() ) {
            static const string msg = "No current backtrack context";
            ScamExpr * rv = ExpressionFactory::makeError(msg);
            cont->run(rv);
        }
        else {
            backtracker->run();
        }
    }


    void apply_trace(ScamExpr * args, Continuation * cont)
    {
        cerr << args->toString() << "\n";
        cont->run(args);
    }

}
