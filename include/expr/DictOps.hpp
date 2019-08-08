#if ! defined(DICTOPS_HPP)
#define DICTOPS_HPP 1

#include "ScamFwd.hpp"
#include "expr/ScamData.hpp"

namespace scam
{
    using KeyVec = ScamData::DictKeyData;
    using ValVec = ScamData::DictValueData;

    extern ScamValue dictHas(ScamValue value, ScamValue key);
    extern ScamValue dictGet(ScamValue value, ScamValue key);
    extern ScamValue dictPut(ScamValue value, ScamValue key, ScamValue val);
    extern ScamValue dictRemove(ScamValue value, ScamValue key);
    extern ScamValue getDictKeys(ScamValue value, const KeyVec *& result);
}

#endif
