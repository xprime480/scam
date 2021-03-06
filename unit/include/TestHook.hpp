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
	void markRoots() const override;
	void releaseRoots() override;

    private:
        std::vector<ManagedObject *> roots;
    };
}

#endif
