#include "util/FileUtils.hpp"

#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "input/PortCharStream.hpp"
#include "port/FilePort.hpp"
#include "util/ReadEvalStream.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern bool fileExists(string fullpath);
    extern string makePath(string dirname, string filename);

    extern ScamValue initPath();
    extern ScamValue convertPath(char const * path);
    extern ScamValue defaultPath();
    extern string getNextElement(char const *& path);
    extern ScamValue pushPath(string filename);
    static ScamValue dynamicPath = initPath();
}

string scam::findFileOnPath(const string & filename)
{
    if ( '/' == filename.at(0) ) {
        if ( fileExists(filename) ) {
            return filename;
        }
    }
    else {
        size_t n = length(dynamicPath);
        for ( size_t i = 0 ; i < n ; ++i ) {
            string fullpath =
                makePath(asString(nthcar(dynamicPath, i)), filename);

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
    ScamValue save = dynamicPath;
    dynamicPath = pushPath(fullpath);
    ScamValue last = helper.run();
    dynamicPath = save;
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

    string makePath(string dirname, string filename)
    {
        stringstream s;
        s << dirname << "/" << filename;
        return s.str();
    }

    ScamValue initPath()
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
        static const char * path { ".:.." };
        ScamValue rv = convertPath(path);
        return rv;
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
            const char c = *path;
            if ( ! c ) {
                break;
            }

            ++path;
            if ( ':' == c ) {
                break;
            }

            s << c;
        }

        string pe = s.str();
        return pe;
    }

    ScamValue pushPath(string filename)
    {
        ScamValue rv = dynamicPath;

        auto iter = filename.find_last_of("/");
        if ( string::npos != iter ) {
            ScamValue newElement = makeString(filename.substr(0, iter));
            ExprVec dp;
            if ( isVector(dynamicPath) ) {
                dp = dynamicPath->vectorData();
            }

            dp.push_back(newElement);
            rv = makeVector(dp);
        }

        return rv;
    }
}
