#if ! defined(GLOBALID_HPP)
#define GLOBALID_HPP 1

#include <string>

namespace scam
{
    struct GlobalId
    {
        static std::string makeName(const char * tag);

        GlobalId();
        const unsigned long id;
    };

    template <typename T>
    T & operator<<(T & os, GlobalId & gid)
    {
        os << "gid:" << gid.id;
        return os;
    }
}

#endif
