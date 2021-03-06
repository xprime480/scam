#include "Backtracker.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamException.hpp"
#include "util/GlobalId.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

//#define TRACE_BACKTRACKER true

namespace
{
    extern ScamValue getNoMore();
}

Backtracker::Backtracker(char const * id, Backtracker * parent)
    : name(GlobalId::makeName(id))
    , parent(parent)
{
}

Backtracker::~Backtracker()
{
};

void Backtracker::mark()
{
    if ( ! isMarked() ) {
        ManagedObject::mark();
        if ( parent ) {
            parent->mark();
        }
    }
}

void Backtracker::dumpStack(Backtracker * bt)
{
    if ( ! bt ) {
        cerr << "<no backtracker stack>\n";
        return;
    }

    for ( size_t n = 1 ; bt ; ++n, bt = bt->getParent() ) {
        cerr << "[" << n << "]\t" << bt->id() << "\n";
    }
}

string Backtracker::safeID(Backtracker * bt)
{
    if ( ! bt ) {
        return "<null backtracker>";
    }
    return bt->id();
}

void Backtracker::safeRun(Backtracker * bt, Continuation * cont)
{
    if ( bt ) {
        bt->run();
    }
    else {
        cont->handleValue(getNoMore());
    }
}

void Backtracker::run()
{
#if defined(TRACE_BACKTRACKER)
    cerr << "Executing backtracker " << name << "\n";
#endif
}

string Backtracker::id() const
{
    return name;
}

Backtracker * Backtracker::getParent() const
{
    return parent;
}

void Backtracker::runParent(Continuation * cont) const
{
    if ( ! parent ) {
        cont->handleValue(getNoMore());
    }
    else {
        parent->run();
    }
}

namespace
{
    ScamValue getNoMore()
    {
        ScamValue nomore = makeError("No more choices");
        nomore->errorCategory() = valuesCategory;
        ScamValue test = nomore->setMeta("amb-error", makeNull());
        if ( isUnhandledError(test) ) {
            throw ScamException(writeValue(test));
        }
        return nomore;
    }
}
