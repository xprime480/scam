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
        Continuation(char const * name, ScamEngine * engine);

    private:
        static Continuation *
        makeInstance(char const * name, ScamEngine * engine);

    public:
        virtual ~Continuation();

        virtual void handleValue(ScamValue value);
        std::string id() const;

    private:
        std::string const name;

    protected:
        ScamEngine * engine;

    private:
        static std::string makeName(char const * id);
    };
}

#endif
