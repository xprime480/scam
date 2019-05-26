#if ! defined(DICTOPS_HPP)
#define DICTOPS_HPP 1

#include "ScamFwd.hpp"

#include <vector>

namespace scam
{
    using KeyVec = std::vector<ScamValue>;
    using ValVec = std::vector<ScamValue>;

    extern bool dictHas(ScamValue value, ScamValue key);
    extern ScamValue dictGet(ScamValue value, ScamValue key);
    extern ScamValue dictPut(ScamValue value, ScamValue key, ScamValue val);
    extern ScamValue dictRemove(ScamValue value, ScamValue key);
    extern const KeyVec & getDictKeys(ScamValue value);
}


#endif
