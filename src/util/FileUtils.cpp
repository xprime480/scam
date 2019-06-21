#include "util/FileUtils.hpp"

#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "input/PortCharStream.hpp"
#include "port/FilePort.hpp"
#include "util/ReadEvalStream.hpp"

#if 0
#include "util/FileUtils.hpp"

#include <string>
#include <fstream>
#endif

using namespace scam;
using namespace std;

namespace
{
    extern bool fileExists(string fullpath);
    extern ScamValue getPath();
    extern ScamValue defaultPath();
    extern ScamValue convertPath(char const * path);
    extern string getNextElement(char const *& path);
    extern string makePath(string dirname, string filename);
}

string scam::findFileOnPath(const string & filename)
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

ScamValue scam::loadEvalFile(const string & fullpath, ScamEngine * engine)
{
    FilePort * port = new FilePort(fullpath.c_str(), ScamPort::Readable);
    ScamValue value = makePort(port);
    PortCharStream stream(value);
    ReadEvalStream helper(engine, stream);
    ScamValue last = helper.run();
    return last;
}

namespace
{
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

    ScamValue getPath()
    {
        ScamValue rv = makeNothing();

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
