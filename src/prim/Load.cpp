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
    extern void fileNotFound(string const & filename, Continuation * cont);
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

        string fullpath = findFileOnPath(filename);
        if ( fullpath.empty() ) {
            fileNotFound(filename, cont);
            return;
        }

        ScamValue last = loadEvalFile(fullpath);
        ScamEngine::getEngine().setLoaded(filename);
        cont->handleValue(last);
    }
}

namespace
{
    void fileNotFound(string const & filename, Continuation * cont)
    {
        ScamValue err = makeFileError("File Error, file not found (%{0})",
                                      makeSymbol(filename));
        err->errorCategory() = fileCategory;
        ScamEngine::getEngine().handleError(err);
    }

    ScamValue makeFileError(const char * text, ScamValue irr0)
    {
        ScamValue rv = makeError(text, irr0);
        rv->errorCategory() = fileCategory;
        return rv;
    }
}
