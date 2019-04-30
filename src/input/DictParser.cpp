#include "input/DictParser.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

const ScamKeyword *
DictParser::getOp = ExpressionFactory::makeKeyword(":get", false);

const ScamKeyword *
DictParser::putOp = ExpressionFactory::makeKeyword(":put", false);

const ScamKeyword *
DictParser::lenOp = ExpressionFactory::makeKeyword(":length", false);

const ScamKeyword *
DictParser::remOp = ExpressionFactory::makeKeyword(":remove", false);

const ScamKeyword *
DictParser::hasOp = ExpressionFactory::makeKeyword(":has", false);

