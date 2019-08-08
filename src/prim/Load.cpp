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
    extern void fileNotFound(string const & filename,
                             Continuation * cont,
                             ScamEngine * engine);

    extern ScamValue makeFileError(const char * text, ScamValue irr0);
}

void scam::applyLoad(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * name = "load";
    StringParameter p0;
    if ( argsToParms(args, engine, name, p0) ) {
        string filename = asString(p0.value);
        if ( engine->isLoaded(filename) ) {
            ScamValue err =
                makeFileError("File %{0} already loaded", p0.value);
            engine->handleError(err);
            return;
        }

        string fullpath = findFileOnPath(filename);
        if ( fullpath.empty() ) {
            fileNotFound(filename, cont, engine);
            return;
        }

        ScamValue last = loadEvalFile(fullpath, engine);
        engine->setLoaded(filename);
        cont->handleValue(last);
    }
}

namespace
{
    void fileNotFound(string const & filename,
                      Continuation * cont,
                      ScamEngine * engine)
    {
        ScamValue err = makeFileError("File Error, file not found (%{0})",
                                      makeSymbol(filename));
        err->errorCategory() = fileCategory;
        engine->handleError(err);
    }

    ScamValue makeFileError(const char * text, ScamValue irr0)
    {
        ScamValue rv = makeError(text, irr0);
        rv->errorCategory() = fileCategory;
        return rv;
    }
}
