#include "ScamRepl.hpp"

#include "ScamException.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

#include <iostream>

using namespace scam;
using namespace std;

ScamRepl::ScamRepl()
    : parser(tokenizer)
{
}

int ScamRepl::run()
{
    banner();
    engine.reset(true);
    if ( ! load_prelude() ) {
        return 1;
    }

    return repl();
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

    if ( isNothing(expr) || error(expr) ) {
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
    for ( ;; ) {
        ScamValue form = read();
        if ( isNothing(form) ) {
            break;
        }
        ScamValue value = eval(form);
        print(value);
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

            if ( ! isNothing(form) && ! error(form) ) {
                tokenizer.flush();
                return form;
            }
            if ( error(form) && ! form->hasMeta("partial") ) {
                tokenizer.flush();
                return form;
            }

            tokenizer.restart();
            cout << "   ?> ";
        }

        if ( ! cin.good() ) {
            cerr << "End of input detected\n";
            return makeNothing();
        }

        string line;
        getline(cin, line);
        if ( ! checkInternal(line) ) {
            return makeNothing();
        }
        tokenizer.bufferInput(line);
    }
}

ScamValue ScamRepl::eval(ScamValue form)
{
    try {
        return engine.eval(form);
    }
    catch ( ScamException e ) {
        ScamValue err = makeErrorExtended("Caught exception: ", e.getMessage());
        return engine.handleError(err);
    }
}

void ScamRepl::print(ScamValue value)
{
    cerr << writeValue(value) << "\n";
}

bool ScamRepl::checkInternal(string & line)
{
    if ( line.empty() ) {
        return true;
    }

    if ( '~' != line.at(0) ) {
        return true;
    }

    string cmd = line;
    line = "";

    if ( cmd.size() < 2 ) {
        cerr << "invalid command sequence: " << cmd << "\n";
        return true;
    }

    const char c = cmd.at(1);
    if ( 'q' == c || 'Q' == c ) {
        return false;
    }

    cerr << "unknown command sequence: " << cmd << "\n";
    return true;
}
