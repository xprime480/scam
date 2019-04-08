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

        void setManaged(bool value) const;
        bool isManaged() const;

        virtual void mark() const;
        bool isMarked() const;
        void unmark() const;

    private:
        mutable bool managed;
        mutable bool marked;
    };
}

#endif
