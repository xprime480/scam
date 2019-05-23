#if ! defined(VALUEWRITER_HPP)
#define VALUEWRITER_HPP 1

#include <string>

namespace scam
{
    class ScamData;

    extern std::string writeValue(const ScamData * data);
}

#endif
