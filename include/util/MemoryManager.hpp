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

        void setSize(size_t size);
        void addHook(std::function<void(void)> & hook);

        void gc();
        void reset();

        size_t getCreateCount() const;
        size_t getCurrentCount() const;

    private:
      size_t arena_size;
      std::vector<std::unique_ptr<ManagedObject>> arena;

      std::vector<std::function<void(void)>> hooks;

      size_t createCount;

      void mark() const;
      void sweep();
      void unmark() const;
    };

    // for now, a global variable!
    //
    extern MemoryManager standardMemoryManager;
}

#endif
