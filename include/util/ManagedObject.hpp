#if ! defined(MANAGEDOBJECT_H)
#define MANAGEDOBJECT_H 1

#include <memory>

namespace scam
{
    class ManagedObject
    {
    public:
        ManagedObject();
        virtual ~ManagedObject();

        virtual void mark();
        bool isMarked() const;
        void unmark();

    private:
        bool marked;
    };
}

#endif
