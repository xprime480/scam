#if ! defined(BACKTRACKER_H)
#define BACKTRACKER_H

#include "Continuation.hpp"

#include <memory>
#include <string>

namespace scam
{
    class Backtracker;
    using BacktrackHandle = std::shared_ptr<Backtracker>;

    class Backtracker
    {
    public:
        Backtracker(char const * id, BacktrackHandle parent);
        virtual ~Backtracker();

        static void dumpStack(BacktrackHandle bt);
        static std::string safeID(BacktrackHandle bt);

        virtual void run(ContHandle cont);
        std::string id() const;

    protected:
        void runParent(ContHandle) const;
        BacktrackHandle getParent() const;

    private:
        std::string const name;
        BacktrackHandle parent;

        static std::string makeName(char const * id);
    };

}

#endif
