#include "TestHook.hpp"

using namespace scam;

/* This class is obsolete and can be removed */

void TestHook::addRoot(ManagedObject * root)
{
    roots.push_back(root);
}

void TestHook::operator()() const
{
    for ( auto root : roots ) {
        root->mark();
    }
}
