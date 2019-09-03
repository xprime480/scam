#if ! defined(MEMORYMANAGER_H)
#define MEMORYMANAGER_H 1

#include "util/ManagedObject.hpp"

#include <functional>
#include <memory>
#include <set>
#include <vector>

namespace scam
{
    class MemoryManager
    {
    public:
        struct Hook
        {
            virtual void markRoots() const = 0;
            virtual void releaseRoots() = 0;
        };

        static constexpr size_t DEFAULT_SIZE = 2 << 16;

        MemoryManager(size_t size = DEFAULT_SIZE);
        ~MemoryManager();

        template <typename T, typename... Args>
        T * make(Args && ... args)
        {
            T * raw = T::makeInstance(args...);
            if ( raw && raw->isManaged() ) {
                arena.emplace_back(raw);
                ++createCount;
            }
            return raw;
        }

        void addHook(Hook * hook);
        void removeHook(Hook * hook);

        void gc(bool force = false);
        void reset();

        size_t getCreateCount() const;
        size_t getCurrentCount() const;

        bool isSuppressed() const;
        void setSuppressed(bool value);

    private:
        const size_t arena_size;
        std::vector<std::unique_ptr<ManagedObject>> arena;

        std::set<Hook *> hooks;

        size_t createCount;

        bool suppressed;

        void mark();
        void sweep();
        void unmark();
    };
}

#endif
