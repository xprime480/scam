#include "prim/Load.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/ValueFactory.hpp"
#include "util/FileUtils.hpp"
#include "util/MemoryManager.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern ScamValue fileNotFound(string const & filename);
    extern ScamValue makeFileError(const char * text, ScamValue irr0);
}

void scam::applyLoad(ScamValue args, Continuation * cont)
{
    static const char * name = "load";
    StringParameter p0;
    if ( argsToParms(args, name, p0) ) {
        string filename = asString(p0.value);

        if ( ScamEngine::getEngine().isLoaded(filename) ) {
            ScamValue err = makeFileError("File %{0} already loaded", p0.value);
            ScamEngine::getEngine().handleError(err);
            return;
        }

        else {
            ScamValue rv = loadHelper(filename.c_str());
            if ( isUnhandledError(rv) ) {
                ScamEngine::getEngine().handleError(rv);
            }
            else {
                ScamEngine::getEngine().setLoaded(filename);
                cont->handleValue(rv);
            }
        }
    }
}

ScamValue scam::loadHelper(const char * filename)
{
    string fullpath = findFileOnPath(filename);
    if ( fullpath.empty() ) {
        return fileNotFound(filename);
    }

    return loadEvalFile(fullpath);
}

namespace
{
    ScamValue fileNotFound(string const & filename)
    {
        ScamValue err = makeFileError("File Error, file not found (%{0})",
                                      makeString(filename));
        return err;
    }

    ScamValue makeFileError(const char * text, ScamValue irr0)
    {
        ScamValue rv = makeError(text, irr0);
        rv->errorCategory() = fileCategory;
        return rv;
    }
}
