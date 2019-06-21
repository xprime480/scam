#include "prim/Load.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/ValueFactory.hpp"
#include "input/SingletonParser.hpp"
#include "input/TypeParsers.hpp"
#include "util/ArgListHelper.hpp"
#include "util/FileUtils.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern void fileNotFound(string const & filename,
                             Continuation * cont,
                             ScamEngine * engine);

    extern ScamValue makeFileError(const char * text, ScamValue irr0);
}

const ScamValue scam::fileErrorCategory = makeSymbol(":file", false);

void scam::applyLoad(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * myName = "load";

    StringParser * str = standardMemoryManager.make<StringParser>();
    SingletonParser * parser
        = standardMemoryManager.make<SingletonParser>(str);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(filename-string)", args, cont, engine);
        return;
    }

    string filename = asString(parser->get());
    if ( engine->isLoaded(filename) ) {
        ScamValue err =
            makeFileError("File %{0} already loaded", parser->get());
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

namespace
{
    void fileNotFound(string const & filename,
                      Continuation * cont,
                      ScamEngine * engine)
    {
        ScamValue err = makeFileError("File Error, file not found (%{0})",
                                      makeSymbol(filename));
        engine->handleError(err);
    }

    ScamValue makeFileError(const char * text, ScamValue irr0)
    {
        ScamValue rv = makeError(text, irr0);
        rv->errorCategory() = fileErrorCategory;
        return rv;
    }
}
