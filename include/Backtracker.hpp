#if ! defined(BACKTRACKER_H)
#define BACKTRACKER_H

#include "util/ManagedObject.hpp"

#include "util/GlobalId.hpp"

#include <string>

namespace scam
{
    class Continuation;

    class Backtracker : public ManagedObject
    {
    protected:
        Backtracker(char const * id, Backtracker * parent);

    public:
        void mark() override;

        virtual ~Backtracker();

        static void dumpStack(Backtracker * bt);
        static std::string safeID(Backtracker * bt);
        static void safeRun(Backtracker * bt, Continuation * cont);

        virtual void run();
        std::string id() const;

    protected:
        void runParent(Continuation * cont) const;
        Backtracker * getParent() const;

    private:
        std::string const name;
        Backtracker * parent;
    };
}

#endif
