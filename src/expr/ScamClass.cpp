#include "expr/ScamClass.hpp"

#include "Env.hpp"

using namespace scam;
using namespace std;

ScamClass::ScamClass(ClassDefParser * def, Env * capture)
    : ScamData(ScamData::Class)
{
    CLASSDEF(this) = def;
    CLASSENV(this) = capture;
}

ScamClass * ScamClass::makeInstance(ClassDefParser * def, Env * capture)
{
    return new ScamClass(def, capture);
}
