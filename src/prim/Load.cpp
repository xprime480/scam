#include "prim/Load.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "input/PortCharStream.hpp"
#include "input/SingletonParser.hpp"
#include "input/TypeParsers.hpp"
#include "port/FilePort.hpp"
#include "util/ArgListHelper.hpp"
#include "util/ReadEvalStream.hpp"
#include "util/MemoryManager.hpp"

#include <string>
#include <fstream>

using namespace scam;
using namespace std;

namespace
{
    extern string findFileOnPath(string const & filename);
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
    string fullpath = findFileOnPath(filename);
    if ( fullpath.empty() ) {
        fileNotFound(filename, cont);
        return;
    }

    FilePort * port = new FilePort(fullpath.c_str(), ScamPort::Readable);
    ScamValue value = makePort(port);
    PortCharStream stream(value);
    ReadEvalStream helper(engine, stream);
    ScamValue last = helper.run();

    engine->setLoaded(filename);
    cont->run(last);
}

namespace
{
    string findFileOnPath(const string & filename)
    {
        if ( '/' == filename.at(0) ) {
            if ( fileExists(filename) ) {
                return filename;
            }
        }
        else {
            ScamValue path = getPath();

            size_t n = length(path);
            for ( size_t i = 0 ; i < n ; ++i ) {
                string fullpath = makePath(asString(nthcar(path, i)), filename);
                if ( fileExists(fullpath) ) {
                    return fullpath;
                }
            }
        }

        return "";
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
