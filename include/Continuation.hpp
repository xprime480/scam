#if ! defined(CONTINUATION_H)
#define CONTINUATION_H

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"

#include <string>

namespace scam
{
    class Continuation : public ManagedObject
    {
    private:
        friend class MemoryManager;

    protected:
        Continuation(char const * name);

    private:
        static Continuation * makeInstance(char const * name);

    public:
        virtual ~Continuation();

        virtual void run(ExprHandle expr);
        std::string id() const;

    private:
        std::string const name;
        static std::string makeName(char const * id);
    };
}

#endif
