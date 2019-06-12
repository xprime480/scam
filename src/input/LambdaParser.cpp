#include "input/LambdaParser.hpp"

#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "input/CountedListParser.hpp"
#include "input/ParameterListParser.hpp"

using namespace scam;
using namespace std;

LambdaParser::LambdaParser()
    : formList(makeNothing())
{
    MemoryManager & mm = standardMemoryManager;

    formals = mm.make<ParameterListParser>();
}

LambdaParser * LambdaParser::makeInstance()
{
    return new LambdaParser();
}

void LambdaParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();

        formals->mark();
        for ( const auto & f : forms ) {
            f->mark();
        }

        formList->mark();
    }
}

bool LambdaParser::accept(ScamValue expr)
{
    if ( ! ArgParser::accept(expr) ) {
        return false;
    }

    clearValue();

    CountedListParser * temp = getCountedListOfAnythingParser(1, 99999);

    if ( ! temp->accept(expr) ) {
        return false;
    }

    if ( ! formals->accept(temp->get(0)) ) {
        return false;
    }

    const size_t count = temp->size();
    for ( size_t idx = 1u ; idx < count ; ++idx ) {
        forms.push_back(temp->get(idx));
    }
    formList = nthcdr(expr, 0);

    callback(expr);
    return true;
}

void LambdaParser::clearValue()
{
    ArgParser::clearValue();
    forms.clear();
    formList = makeNothing();
}

const ParameterListParser * LambdaParser::getArgs() const
{
    return formals;
}

size_t LambdaParser::getFormCount() const
{
    return forms.size();
}

ScamValue LambdaParser::getForm(size_t idx) const
{
    if ( idx < forms.size() ) {
        return forms[idx];
    }
    return makeNothing();
}

ScamValue LambdaParser::getFormList() const
{
    return formList;
}
