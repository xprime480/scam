#include "prim/Load.hpp"

#include "ScamEngine.hpp"
#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/SingletonParser.hpp"
#include "input/TypeParsers.hpp"
#include "util/ArgListHelper.hpp"
#include "util/ReadEvalString.hpp"

#include <string>
#include <sstream>
#include <fstream>

using namespace scam;
using namespace std;

static const char * myName = "load";

Load::Load(ScamEngine * engine)
    : Primitive(myName)
    , engine(engine)
{
}

Load * Load::makeInstance(ScamEngine * engine)
{
    return new Load(engine);
}

void Load::applyArgs(ExprHandle args, Continuation * cont)
{
    StringParser * str = standardMemoryManager.make<StringParser>();
    SingletonParser * parser = standardMemoryManager.make<SingletonParser>(str);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(filename-string)", args, cont);
        return;
    }

    string filename = ExprWriter::write(parser->get());
    if ( engine->isLoaded(filename) ) {
        ExprHandle err =
            ExpressionFactory::makeError("file \"",
                                         filename,
                                         "\" already loaded");
        cont->run(err);
        return;
    }

    ifstream source;
    if ( ! open_file(source, filename, cont) ) {
        return;
    }

    string data = get_data(source);
    ReadEvalString helper(engine, data);
    ExprHandle last = helper.run();

    engine->setLoaded(filename);
    cont->run(last);
}

bool Load::open_file(ifstream & source,
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
        ExprHandle path = get_path();

        size_t n = path->length();
        for ( size_t i = 0 ; i < n ; ++i ) {
            string fullpath = make_path(ExprWriter::write(path->nthcar(i)),
                                        filename);
            if ( file_exists(fullpath) ) {
                source.open(fullpath);
                return true;
            }
        }
    }

    file_not_found(filename, cont);
    return false;
}

string Load::get_data(ifstream & source)
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

bool Load::file_exists(string fullpath)
{
    ifstream x;
    x.open(fullpath);
    if ( x.good() ) {
        x.close();
        return true;
    }

    return false;
}

void Load::file_not_found(string const & filename, Continuation * cont)
{
    ExprHandle err = ExpressionFactory::makeError("Unable to open file ",
                                                  filename);
    cont->run(err);
}

ExprHandle Load::get_path()
{
    ExprHandle rv = ExpressionFactory::makeNull();

    char const * path = getenv("SCAM_PATH");
    if ( ! path || ! *path ) {
        rv = default_path();
    }
    else {
        rv = convert_path(path);
    }
    return rv;
}

ExprHandle Load::default_path()
{
    return convert_path(".:..");
}

ExprHandle Load::convert_path(char const * path)
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

string Load::next_element(char const *& path)
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

string Load::make_path(string dirname, string filename)
{
    stringstream s;
    s << dirname << "/" << filename;
    return s.str();
}
