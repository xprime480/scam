#if ! defined(MANAGEDOBJECT_H)
#define MANAGEDOBJECT_H 1

namespace scam
{
    class ManagedObject
    {
    public:
        ManagedObject(bool managed = true);
        virtual ~ManagedObject();

        bool isManaged() const;

        virtual void mark() const;
        bool isMarked() const;
        void unmark() const;

    private:
        const   bool managed;
        mutable bool marked;
    };
}

#endif
