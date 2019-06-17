#if ! defined(MEMORYMANAGER_H)
#define MEMORYMANAGER_H 1

#include "util/ManagedObject.hpp"

#include <functional>
#include <memory>
#include <vector>

namespace scam
{
    class MemoryManager
    {
    public:
        struct Hook
        {
            virtual void operator()() const = 0;
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

        void setSize(size_t size);
        void addHook(Hook * hook);
        void removeHook(Hook * hook);

        void gc();
        void reset();

        size_t getCreateCount() const;
        size_t getCurrentCount() const;

    private:
      size_t arena_size;
      std::vector<std::unique_ptr<ManagedObject>> arena;

      std::vector<Hook *> hooks;

      size_t createCount;

      void mark();
      void sweep();
      void unmark();
    };

    // for now, a global variable!
    //
    extern MemoryManager standardMemoryManager;
}

#endif
