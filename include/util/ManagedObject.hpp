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

        virtual void mark();
        bool isMarked() const;
        void unmark();

    private:
        const bool managed;
              bool marked;
    };
}

#endif
