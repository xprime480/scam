#if ! defined(DICTOPS_HPP)
#define DICTOPS_HPP 1

#include "ScamFwd.hpp"
#include "value/ScamData.hpp"

namespace scam
{
    extern ScamValue dictHas(ScamValue value, ScamValue key);
    extern ScamValue dictGet(ScamValue value, ScamValue key);
    extern ScamValue dictPut(ScamValue value, ScamValue key, ScamValue val);
    extern ScamValue dictRemove(ScamValue value, ScamValue key);

    extern ScamValue
    getDictKeys(ScamValue value, const std::vector<ScamValue> *& result);
}

#endif
