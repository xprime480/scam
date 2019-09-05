#include "util/MemoryManager.hpp"

#include <algorithm>
#include <iostream>

using namespace scam;
using namespace std;

MemoryManager::MemoryManager(size_t size)
    : arena_size(size)
    , createCount(0)
    , maxCount(0)
    , suppressed(false)
{
}

MemoryManager::~MemoryManager()
{
    cerr << "Allocation maximum: " << maxCount << "\n";
}

void MemoryManager::addHook(Hook * hook)
{
    hooks.insert(hook);
}

void MemoryManager::removeHook(Hook * hook)
{
    auto it = find(hooks.begin(), hooks.end(), hook);
    if ( it != hooks.end() ) {
        hooks.erase(it);
    }
}

void MemoryManager::gc(bool force)
{
    if ( suppressed ) {
        return;
    }

    const size_t count { getCurrentCount() };
    if ( force || (count >= arena_size) ) {
        mark();
        sweep();
        unmark();
    }
}

void MemoryManager::reset()
{
    for ( const auto & hook : hooks ) {
        hook->releaseRoots();
    }

    arena.clear();
    hooks.clear();
    createCount = 0u;
}

size_t MemoryManager::getCreateCount() const
{
    return createCount;
}

size_t MemoryManager::getCurrentCount() const
{
    return arena.size();
}

bool MemoryManager::isSuppressed() const
{
    return suppressed;
}

void MemoryManager::setSuppressed(bool value)
{
    suppressed = value;
}

void MemoryManager::mark()
{
    for ( const auto & hook : hooks ) {
        hook->markRoots();
    }
}

void MemoryManager::sweep()
{
    const size_t count { getCurrentCount() };
    size_t copyToIndex { 0 };

    for ( size_t i = 0 ; i < count ; ++i ) {
        if ( arena[i]->isMarked() ) {
            if ( i != copyToIndex ) {
                arena[copyToIndex] = std::move(arena[i]);
            }
            ++copyToIndex;
        }
        else {
            arena[i].reset();
        }
    }

    auto iter = arena.begin();
    std::advance(iter, copyToIndex);
    arena.erase(iter, arena.end());
}

void MemoryManager::unmark()
{
    for ( auto & p : arena ) {
        p->unmark();
    }
}

