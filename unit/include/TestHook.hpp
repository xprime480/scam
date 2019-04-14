#if ! defined(TESTHOOK_HPP)
#define TESTHOOK_HPP 1

#include "util/ManagedObject.hpp"
#include "util/MemoryManager.hpp"

#include <vector>

namespace scam
{
    class TestHook : public MemoryManager::Hook
    {
    public:
        void addRoot(ManagedObject * root);
        void operator()() const override;

    private:
        std::vector<ManagedObject *> roots;
    };
}

#endif
