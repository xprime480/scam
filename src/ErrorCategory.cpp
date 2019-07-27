#include "ErrorCategory.hpp"

#include "expr/ValueFactory.hpp"

using namespace scam;

const ScamValue scam::implCategory   = makeSymbol(":not-implemented", false);
const ScamValue scam::argsCategory   = makeSymbol(":args", false);
const ScamValue scam::userCategory   = makeSymbol(":user", false);
const ScamValue scam::envCategory    = makeSymbol(":env", false);
const ScamValue scam::syntaxCategory = makeSymbol(":syntax", false);
