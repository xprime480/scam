#include "ErrorCategory.hpp"

#include "expr/ValueFactory.hpp"

using namespace scam;

const ScamValue scam::implCategory   = makeKeyword(":not-implemented", false);
const ScamValue scam::argsCategory   = makeKeyword(":args", false);
const ScamValue scam::userCategory   = makeKeyword(":user", false);
const ScamValue scam::envCategory    = makeKeyword(":env", false);
const ScamValue scam::syntaxCategory = makeKeyword(":syntax", false);
const ScamValue scam::evalCategory   = makeKeyword(":eval", false);
