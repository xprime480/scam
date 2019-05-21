#include "expr/ScamClassAdapter.hpp"

#include "ScamException.hpp"
#include "expr/ScamClass.hpp"
#include "input/ClassDefParser.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamClassAdapter::ScamClassAdapter(const ScamClass * cls)
    : cls(cls)
{
}

const ScamSymbol * ScamClassAdapter::getBase() const
{
    return CLASSDEF(cls->data)->getBase();
}

size_t ScamClassAdapter::getVarCount() const
{
    return CLASSDEF(cls->data)->getVarCount();
}

const ScamSymbol * ScamClassAdapter::getVar(size_t idx) const
{
    return CLASSDEF(cls->data)->getVar(idx);
}

size_t ScamClassAdapter::getMethodCount() const
{
    return CLASSDEF(cls->data)->getMethodCount();
}

const FunctionDefParser * ScamClassAdapter::getMethod(size_t idx) const
{
    return CLASSDEF(cls->data)->getMethod(idx);
}

Env * ScamClassAdapter::getCapture() const
{
    return CLASSENV(cls->data);
}
