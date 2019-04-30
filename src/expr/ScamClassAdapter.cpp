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
    return cls->def->getBase();
}

size_t ScamClassAdapter::getVarCount() const
{
    return cls->def->getVarCount();
}

const ScamSymbol * ScamClassAdapter::getVar(size_t idx) const
{
    return cls->def->getVar(idx);
}

size_t ScamClassAdapter::getMethodCount() const
{
    return cls->def->getMethodCount();
}

const FunctionDefParser * ScamClassAdapter::getMethod(size_t idx) const
{
    return cls->def->getMethod(idx);
}

Env * ScamClassAdapter::getCapture() const
{
    return cls->capture;
}
