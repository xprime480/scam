#if ! defined(MEMORYMANAGER_H)
#define MEMORYMANAGER_H 1

#include <functional>
#include <memory>
#include <vector>

#include "util/ManagedObject.hpp"

namespace scam
{
    class MemoryManager
    {
    public:
        MemoryManager(size_t size = 2 << 16);
        ~MemoryManager();

        template <typename T, typename... Args>
        T * make(Args && ... args)
        {
            T * raw = T::makeInstance(args...);
            arena.emplace_back(raw);
            ++createCount;
            return raw;
        }

        void addHook(std::function<void(void)> & hook);

        void gc();

        size_t getCreateCount() const;
        size_t getCurrentCount() const;

    private:
      const size_t arena_size;
      std::vector<std::unique_ptr<ManagedObject>> arena;

      std::vector<std::function<void(void)>> hooks;

      size_t createCount;

      void mark() const;
      void sweep();
      void unmark() const;
    };
}

#endif
