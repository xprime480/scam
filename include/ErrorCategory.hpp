#if ! defined(ERRORCATEGORY_HPP)
#define ERRORCATEGORY_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    /**
     * Provide values for error categories.  They may be recovered
     * in schem code with
     *
     *        (error-category error-object)
     */
    extern const ScamValue implCategory;
    extern const ScamValue argsCategory;
    extern const ScamValue userCategory;
    extern const ScamValue envCategory;
}

#endif
