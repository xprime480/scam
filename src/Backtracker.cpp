#include "Backtracker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamData.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

//#define TRACE_BACKTRACKER true

namespace
{
    static unsigned counter { 0 };
    static ScamValue nomore =
        ExpressionFactory::makeError("No more choices", false);
    static bool init =
        (nomore->setMeta("amb-error", ExpressionFactory::makeNil()),
         true);
}

Backtracker::Backtracker(char const * id, Backtracker * parent)
    : name(makeName(id))
    , parent(parent)
{
  if ( ! init ) { id = nullptr; } // compiler pacifier
#if defined(TRACE_BACKTRACKER)
    cerr << "Creating backtracker " << name << "\n";
    cerr << "\tParent is " << parent << "\n";
#endif
}

Backtracker::~Backtracker()
{
#if defined(TRACE_BACKTRACKER)
    cerr << "Deleting backtracker " << name << "\n";
#endif
};

void Backtracker::mark() const
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
        cont->run(nomore);
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
        cont->run(nomore);
    }
    else {
        parent->run();
    }
}

string Backtracker::makeName(char const * id)
{
    stringstream s;
    s << (++counter) << " {" << id << "}";
    return s.str();
}
