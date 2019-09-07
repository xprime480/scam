#include "ScamRepl.hpp"

#include "ScamException.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/FileUtils.hpp"

#include "ReplHandler.hpp"

#include <iostream>

using namespace scam;
using namespace std;

ScamRepl::ScamRepl(int argc, char ** argv)
    : engine(ScamEngine::getEngine())
    , parser(tokenizer)
    , testmode(false)
    , done(false)
{
    readArgs(argc, argv);
}

int ScamRepl::run()
{
    Handler * handler = engine.getMemoryManager().make<ReplHandler>();

    engine.reset(true);
    engine.pushHandler(handler);

    int status = load_preloads();
    if ( ! testmode && (0 == status) ) {
        status = repl();
    }

    engine.popHandler();
    return status;
}

void ScamRepl::readArgs(int argc, char ** argv)
{
    bool interactive = true;
    while ( --argc ) {
        const char * arg = *++argv;
        if ( '-' == arg[0] ) {
            switch ( arg[1] ) {
            case 't':
                testmode = true;
                interactive = false;
                break;

            case 'q':
                interactive = false;
                break;

            default:
                cerr << "unknown flag: " << arg << "\n";
                break;
            }
        }
        else {
            preloads.push_back(arg);
        }
    }

    if ( interactive ) {
        banner();
    }
}

void ScamRepl::banner() const
{
    cout << "Welcome to scam!\n";
    cout << "\n";
    cout << "Type your commands at the 'scam> ' prompt\n";
    cout << "Expressions will be evaluated and the results printed.\n";
    cout << "If your input has multiple expressions, each result\n";
    cout << "\tis printed on a new line.\n";
    cout << "If your input is incomplete, you will see the '?>' prompt.\n";
    cout << "\n";
    cout << "Type ~q to exit\n";
    cout << "\n";
}

int ScamRepl::load_preloads()
{
    for ( const auto & f : preloads ) {
        ScamValue result = evaluateFile(f);
        int check = checkResult(f, result);
        if ( 0 != check ) {
            return check;
        }
    }

    return 0;
}

ScamValue ScamRepl::evaluateFile(const string & name)
{
    ScamValue rv = makeNothing();

    try {
        string fullpath = findFileOnPath(name);
        if ( fullpath.empty() ) {
            rv = makeError("File %{0} not found on path", makeString(name));
        }
        else {
            rv = loadEvalFile(fullpath);
        }
    }
    catch ( ScamException e )  {
        rv = makeError("ScamException %{0} evaluating file %{1}",
                       makeString(e.getMessage()),
                       makeString(name));
    }
    catch ( exception e ) {
        rv = makeError("exception %{0} evaluating file %{1}",
                       makeString(e.what()),
                       makeString(name));
    }
    catch ( ... ) {
        rv = makeError("Unknown error evaluating file %{0}",
                       makeString(name));
    }

    return rv;
}

int ScamRepl::checkResult(const string & file, ScamValue result)
{
    if ( isNothing(result) || isError(result) ) {
        cerr << "Unable to read file " << file << "\n";
        if ( isError(result) ) {
            cerr << writeValue(result) << "\n";
        }
        return 1;
    }
    else if ( isInteger(result) ) {
        return asInteger(result);
    }
    else {
        return truth(result) ? 0 : 1;
    }

    return -1;
}

int ScamRepl::repl()
{
    while ( ! done ) {
        ScamValue form = read();
        if ( isAnything(form) ) {
            ScamValue value = eval(form);
            if ( isAnything(value) ) {
                print(value);
            }
        }
    }

    return 0;
}

ScamValue ScamRepl::read()
{
    while ( true ) {
        if ( tokenizer.empty() ) {
            cout << "scam> ";
        }
        else {
            ScamValue form = parser.parseExpr();

            if ( ! isNothing(form) && ! isError(form) ) {
                tokenizer.flush();
                return form;
            }
            if ( isError(form) ) {
                ScamValue test = form->hasMeta("partial");
                if ( isError(test) ) {
                    throw ScamException(writeValue(test));
                }
                if ( ! truth(test) ) {
                    form = engine.handleError(form);
                    tokenizer.flush();
                    return form;
                }
            }

            tokenizer.restart();
            cout << "   ?> ";
        }

        if ( ! cin.good() ) {
            cerr << "End of input detected\n";
            done = true;
            return makeNothing();
        }

        string line;
        getline(cin, line);
        checkInternal(line);
        if ( done ) {
            return makeNothing();
        }
        tokenizer.bufferInput(line);
    }
}

ScamValue ScamRepl::eval(ScamValue form)
{
    ScamValue rv = makeNull();
    try {
        rv = engine.eval(form);
    }
    catch ( ScamException e ) {
        ScamValue temp = makeString(e.getMessage());
        rv = makeError("Caught exception", temp);
    }

    if ( isUnhandledError(rv) ) {
        rv = engine.handleError(rv);
    }

    return rv;

}

void ScamRepl::print(ScamValue value)
{
    cerr << writeValue(value) << "\n";
}

void ScamRepl::checkInternal(string & line)
{
    if ( line.empty() ) {
        return;
    }

    if ( '~' != line.at(0) ) {
        return;
    }

    string cmd = line;
    line = "";

    if ( cmd.size() < 2 ) {
        cerr << "invalid command sequence: " << cmd << "\n";
        return;
    }

    const char c = cmd.at(1);
    if ( 'q' == c || 'Q' == c ) {
        done = true;
        return;
    }

    cerr << "unknown command sequence: " << cmd << "\n";
}
