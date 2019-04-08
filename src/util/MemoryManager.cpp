
#include "util/MemoryManager.hpp"

using namespace scam;

MemoryManager scam::standardMemoryManager(MemoryManager::DEFAULT_SIZE);

MemoryManager::MemoryManager(size_t size)
    : arena_size(size)
    , createCount(0)
{
}

MemoryManager::~MemoryManager()
{
}

void MemoryManager::setSize(size_t size)
{
    arena_size = size;
}

void MemoryManager::addHook(std::function<void(void)> & hook)
{
    hooks.push_back(hook);
}

void MemoryManager::gc()
{
    const size_t count { getCurrentCount() };
    if ( count < arena_size ) {
        return;
    }

    mark();
    sweep();
    unmark();
}

void MemoryManager::reset()
{
    arena.clear();
    hooks.clear();
    setSize(DEFAULT_SIZE);
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

void MemoryManager::mark() const
{
    for ( const auto & hook : hooks ) {
        hook();
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

void MemoryManager::unmark() const
{
    for ( auto & p : arena ) {
        p->unmark();
    }
}

