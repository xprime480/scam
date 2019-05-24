#if ! defined(GLOBALID_HPP)
#define GLOBALID_HPP 1

namespace scam
{
    struct GlobalId
    {
        GlobalId() : id(++counter) {}
        const long id;
    private:
        static long counter;
    };

    template <typename T>
    T & operator<<(T & os, GlobalId & gid)
    {
        os << "gid:" << gid.id;
        return os;
    }
}

#endif
