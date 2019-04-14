#include "TestHook.hpp"

using namespace scam;

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
