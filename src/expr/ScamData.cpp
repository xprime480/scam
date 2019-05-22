#include "expr/ScamData.hpp"

using namespace scam;
using namespace std;

ScamData::ScamData(unsigned long type)
    : type(type)
{
    switch ( type ) {
    case ScamData::Error:
    case ScamData::Keyword:
    case ScamData::String:
    case ScamData::Symbol:
    case ScamData::SpecialForm:
    case ScamData::Primitive:
        STRVALP(*this) = new string;
        break;

    case ScamData::ByteVector:
        BYTEVECTORP(*this) = new vector<unsigned char>;

        break;

    case ScamData::Dict:
        DICTKEYSP(*this) = new vector<ExprHandle>;
        DICTVALSP(*this) = new vector<ExprHandle>;
        break;

    case ScamData::Vector:
        VECTORP(*this) = new vector<ExprHandle>;
        break;

    default:
        break;
    }
}

ScamData::~ScamData()
{
    switch ( type ) {
    case ScamData::Error:
    case ScamData::Keyword:
    case ScamData::String:
    case ScamData::Symbol:
    case ScamData::SpecialForm:
    case ScamData::Primitive:
        delete STRVALP(*this);
        break;

    case ScamData::ByteVector:
        delete BYTEVECTORP(*this);

        break;

    case ScamData::Dict:
        delete DICTKEYSP(*this);
        delete DICTVALSP(*this);
        break;

    case ScamData::Vector:
        delete VECTORP(*this);
        break;

    default:
        break;
    }
}

