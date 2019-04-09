#if ! defined(CONTINUATION_H)
#define CONTINUATION_H

#include "util/ManagedObject.hpp"

#include <memory>
#include <string>

namespace scam
{
    class ScamExpr;
    class MemoryManager;

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

        virtual void run(ScamExpr * expr);
        std::string id() const;

    private:
        std::string const name;
        static std::string makeName(char const * id);
    };
}

#endif
