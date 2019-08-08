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
    extern const ScamValue argsCategory;
    extern const ScamValue dictCategory;
    extern const ScamValue envCategory;
    extern const ScamValue evalCategory;
    extern const ScamValue fileCategory;
    extern const ScamValue implCategory;
    extern const ScamValue readCategory;
    extern const ScamValue syntaxCategory;
    extern const ScamValue userCategory;
}

#endif
