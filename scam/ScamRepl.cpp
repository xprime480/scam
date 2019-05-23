#include "ScamRepl.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

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
    ExprHandle expr = parser.parseExpr();
    tokenizer.flush();

    if ( expr->isNull() || expr->error() ) {
        cerr << "Unable to read the prelude\n";

        if ( expr->isNull() ) {
            return false;
        }

        cerr << ExprWriter::write(expr) << "\n";
        return false;
    }

    ExprHandle rv = eval(expr);
    return rv->isInteger() && 1 == rv->asInteger();
}

int ScamRepl::repl()
{
    for ( ;; ) {
        ExprHandle form = read();
        if ( form->isNull() ) {
            break;
        }
        ExprHandle value = eval(form);
        print(value);
    }

    return 0;
}

ExprHandle ScamRepl::read()
{
    while ( true ) {
        if ( tokenizer.empty() ) {
            cout << "scam> ";
        }
        else {
            ExprHandle form = parser.parseExpr();

            if ( ! form->isNull() && ! form->error() ) {
                tokenizer.flush();
                return form;
            }
            if ( form->error() && ! form->hasMeta("partial") ) {
                tokenizer.flush();
                return form;
            }

            tokenizer.restart();
            cout << "   ?> ";
        }

        if ( ! cin.good() ) {
            cerr << "End of input detected\n";
            return ExpressionFactory::makeNull();
        }

        string line;
        getline(cin, line);
        if ( ! checkInternal(line) ) {
            return ExpressionFactory::makeNull();
        }
        tokenizer.bufferInput(line);
    }
}

ExprHandle ScamRepl::eval(ExprHandle form)
{
    try {
        return engine.eval(form);
    }
    catch ( ScamException e ) {
        return ExpressionFactory::makeError("Caught exception: ",
                                            e.getMessage());
    }
}

void ScamRepl::print(ExprHandle value)
{
    cerr << ExprWriter::write(value) << "\n";
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
