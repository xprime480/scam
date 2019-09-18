#include "ErrorCategory.hpp"

#include "value/ValueFactory.hpp"

using namespace scam;

const ScamValue scam::argsCategory   = makeKeyword(":args", false);
const ScamValue scam::dictCategory   = makeKeyword(":dict", false);
const ScamValue scam::envCategory    = makeKeyword(":env", false);
const ScamValue scam::evalCategory   = makeKeyword(":eval", false);
const ScamValue scam::fileCategory   = makeKeyword(":file", false);
const ScamValue scam::implCategory   = makeKeyword(":not-implemented", false);
const ScamValue scam::importCategory = makeKeyword(":import", false);
const ScamValue scam::internalCategory = makeKeyword(":internal", false);
const ScamValue scam::readCategory   = makeKeyword(":read", false);
const ScamValue scam::scanCategory   = makeKeyword(":scan", false);
const ScamValue scam::syntaxCategory = makeKeyword(":syntax", false);
const ScamValue scam::userCategory   = makeKeyword(":user", false);
const ScamValue scam::valuesCategory = makeKeyword(":values", false);
