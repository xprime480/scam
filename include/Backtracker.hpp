#if ! defined(BACKTRACKER_H)
#define BACKTRACKER_H

#include "Continuation.hpp"

#include <memory>
#include <string>

namespace scam
{
    class Backtracker
    {
    public:
        Backtracker(char const * id);
        virtual ~Backtracker();

        virtual void run(ContHandle cont);
        std::string id() const;

    private:
        std::string const name;
        static std::string makeName(char const * id);
    };

    using BacktrackHandle = std::shared_ptr<Backtracker>;
}

#endif
