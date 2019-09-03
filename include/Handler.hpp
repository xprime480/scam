#if ! defined(HANDLER_HPP)
#define HANDLER_HPP 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"

#include <string>

namespace scam
{
    class Handler : public ManagedObject
    {
    private:
        friend class MemoryManager;

    protected:
        Handler(const char * name);

    private:
        static Handler * makeInstance();

    public:
        virtual ~Handler() {}

        virtual ScamValue handleError(ScamValue err);
        std::string id() const;

    private:
        std::string const name;
        static std::string makeName(char const * id);
    };
}

#endif
