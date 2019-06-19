#include "ScamRepl.hpp"

#include "ScamException.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

#include "ReplHandler.hpp"

#include <iostream>

using namespace scam;
using namespace std;

ScamRepl::ScamRepl()
    : parser(tokenizer)
    , done(false)
{
}

int ScamRepl::run()
{
    Handler * handler = standardMemoryManager.make<ReplHandler>();

    banner();
    engine.reset(true);
    engine.pushHandler(handler);
    if ( ! load_prelude() ) {
        return 1;
    }

    int rv = repl();
    engine.popHandler();
    return rv;
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

bool ScamRepl::load_prelude()
{
    tokenizer.bufferInput("(load \"lib/prelude.scm\")");
    ScamValue expr = parser.parseExpr();
    tokenizer.flush();

    if ( isNothing(expr) || isError(expr) ) {
        cerr << "Unable to read the prelude\n";

        if ( isNothing(expr) ) {
            return false;
        }

        cerr << writeValue(expr) << "\n";
        return false;
    }

    ScamValue rv = eval(expr);
    return isInteger(rv) && 1 == asInteger(rv);
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
            if ( isError(form) && ! form->hasMeta("partial") ) {
                form = engine.handleError(form);
                tokenizer.flush();
                return form;
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
