#include "prim/Load.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "input/SingletonParser.hpp"
#include "input/TypeParsers.hpp"
#include "util/ArgListHelper.hpp"
#include "util/ReadEvalString.hpp"

#include <string>
#include <sstream>
#include <fstream>

using namespace scam;
using namespace std;

namespace
{
    extern bool openFile(ifstream & source,
                          string const & filename,
                          Continuation * cont);

    extern string getData(ifstream & source);
    extern bool fileExists(string fullpath);
    extern void fileNotFound(string const & filename, Continuation * cont);
    extern ScamValue getPath();
    extern ScamValue defaultPath();
    extern ScamValue convertPath(char const * path);
    extern string getNextElement(char const *& path);
    extern string makePath(string dirname, string filename);
}

void scam::applyLoad(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * myName = "load";

    StringParser * str = standardMemoryManager.make<StringParser>();
    SingletonParser * parser
        = standardMemoryManager.make<SingletonParser>(str);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(filename-string)", args, cont);
        return;
    }

    string filename = asString(parser->get());
    if ( engine->isLoaded(filename) ) {
        ScamValue err =
            makeErrorExtended("file \"", filename, "\" already loaded");
        cont->run(err);
        return;
    }
    
    ifstream source;
    if ( ! openFile(source, filename, cont) ) {
        return;
    }

    string data = getData(source);
    ReadEvalString helper(engine, data);
    ScamValue last = helper.run();

    engine->setLoaded(filename);
    cont->run(last);
}

namespace
{
    bool
    openFile(ifstream & source, string const & filename, Continuation * cont)
    {
        if ( '/' == filename.at(0) ) {
            if ( fileExists(filename) ) {
                source.open(filename);
                return true;
            }
        }
        else {
            ScamValue path = getPath();

            size_t n = length(path);
            for ( size_t i = 0 ; i < n ; ++i ) {
                string fullpath = makePath(asString(nthcar(path, i)), filename);
                if ( fileExists(fullpath) ) {
                    source.open(fullpath);
                    return true;
                }
            }
        }

        fileNotFound(filename, cont);
        return false;
    }

    string getData(ifstream & source)
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

    bool fileExists(string fullpath)
    {
        ifstream x;
        x.open(fullpath);
        if ( x.good() ) {
            x.close();
            return true;
        }

        return false;
    }

    void fileNotFound(string const & filename, Continuation * cont)
    {
        ScamValue err = makeErrorExtended("Unable to open file ", filename);
        cont->run(err);
    }

    ScamValue getPath()
    {
        ScamValue rv = makeNull();

        char const * path = getenv("SCAM_PATH");
        if ( ! path || ! *path ) {
            rv = defaultPath();
        }
        else {
            rv = convertPath(path);
        }
        return rv;
    }

    ScamValue defaultPath()
    {
        return convertPath(".:..");
    }

    ScamValue convertPath(char const * path)
    {
        ExprVec dp;

        while ( *path ) {
            string element = getNextElement(path);
            if ( ! element.empty() ) {
                dp.push_back(makeString(element));
            }
        }

        if ( dp.empty() ) {
            return defaultPath();
        }
        return makeVector(dp);
    }

    string getNextElement(char const *& path)
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

    string makePath(string dirname, string filename)
    {
        stringstream s;
        s << dirname << "/" << filename;
        return s.str();
    }
}
