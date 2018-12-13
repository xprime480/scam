
#include "Backtracker.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static unsigned counter { 0 };
}

Backtracker::Backtracker(char const * id, BacktrackHandle parent)
    : name(makeName(id))
    , parent(parent)
{
    //    cerr << "Creating backtracker " << name << "\n";
}

Backtracker::~Backtracker()
{
    //    cerr << "Deleting backtracker " << name << "\n";
};

void Backtracker::dumpStack(BacktrackHandle bt)
{
    if ( nullptr == bt.get() ) {
        cerr << "<no backtracker stack>\n";
        return;
    }

    for ( size_t n = 1 ; nullptr != bt.get() ; ++n, bt = bt->getParent() ) {
        cerr << "[" << n << "]\t" << bt->id() << "\n";
    }
}

string Backtracker::safeID(BacktrackHandle bt)
{
    if ( nullptr == bt.get() ) {
        return "<null backtracker>";
    }
    return bt->id();
}

void Backtracker::run(ContHandle cont)
{
    //    cerr << "Executing backtracker " << name << "\n";
}

string Backtracker::id() const
{
    return name;
}

BacktrackHandle Backtracker::getParent() const
{
    return parent;
}

void Backtracker::runParent(ContHandle cont) const
{
    if ( nullptr == parent.get() ) {
        static const ExprHandle nomore =
            ExpressionFactory::makeError("No more choices");
        cont->run(nomore.get());
    }
    else {
        parent->run(cont);
    }
}

string Backtracker::makeName(char const * id)
{
    stringstream s;
    s << (++counter) << " {" << id << "}";
    return s.str();
}
