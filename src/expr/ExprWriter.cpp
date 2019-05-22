#include "expr/ExprWriter.hpp"

#include "expr/ScamData.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ScamNumeric.hpp"
#include "input/LambdaParser.hpp"
#include "input/ParameterListParser.hpp"

#include <sstream>

using namespace scam;
using namespace std;

string ExprWriter::write(const ScamData * data)
{
    stringstream s;

    if ( ScamNumeric::isNumeric(data) ) {
        writeNumeric(s, data);
    }
    else {
        switch ( data->type ) {
        case ScamData::Boolean:
            s << (BOOLVAL(data) ? "#t" : "#f");
            break;

        case ScamData::ByteVector:
            writeByteVector(s, data);
            break;

        case ScamData::Character:
            s << "#\\" << CHARVAL(data);
            break;

        case ScamData::Class:
            s << "class";
            break;

        case ScamData::Closure:
            writeClosure(s, data);
            break;

        case ScamData::Cons:
            writeCons(s, data);
            break;

        case ScamData::Dict:
            writeDict(s, data);
            break;

        case ScamData::Cont:
            s << "continuation";
            break;

        case ScamData::Error:
        case ScamData::Keyword:
        case ScamData::String:
        case ScamData::Symbol:
            s << STRVAL(data);
            break;

        case ScamData::Instance:
            s << "instance";
            break;

        case ScamData::Nil:
            s << "()";
            break;

        case ScamData::Null:
            s << "null";
            break;

        case ScamData::Vector:
            writeVector(s, data);
            break;

        case ScamData::SpecialForm:
            s << "Special Form " << STRVAL(data);
            break;

        case ScamData::Primitive:
            s << "Primitive " << STRVAL(data);
            break;

        default:
            s << "don't know how to represent this object, type = "
              << data->type;
            break;
        }
    }

    return s.str() ;
}

void ExprWriter::writeByteVector(stringstream & s, const ScamData * data)
{
    string sep { "" };

    s << "#u8(";
    for ( auto const & e : BYTEVECTOR(data) ) {
        s << sep << (int)e;
        sep = " ";
    }
    s << ")";
}

void ExprWriter::writeClosure(stringstream & s, const ScamData * data)
{
    s << "(";

    if ( MACROLIKE(data) ) {
        s << "macro ";
    }
    else {
        s << "lambda ";
    }
    s << CLOSUREDEF(data)->getArgs()->getValue()->toString();
    s << " ";

    const size_t count = CLOSUREDEF(data)->getFormCount();
    for ( size_t idx = 0 ; idx < count ; ++ idx ) {
        if ( idx > 0 ) {
            s << " ";
        }
        s << CLOSUREDEF(data)->getForm(idx)->toString();
    }

    s << ")";
}

void ExprWriter::writeCons(stringstream & s, const ScamData * data)
{
    s << "(";
    s << CAR(data)->toString();
    ExprHandle next = CDR(data);
    while ( ! next->isNil() ) {
        if ( next->isCons() ) {
            s << " " << next->getCar()->toString();
            next = next->getCdr();
        }
        else {
            s << " . " << next->toString();
            break;
        }
    }
    s << ")";
}

void ExprWriter::writeDict(stringstream & s, const ScamData * data)
{
    s << "{";

    for ( size_t idx = 0 ; idx < DICTKEYS(data).size() ; ++idx ) {
        s << " " << DICTKEYS(data)[idx]->toString()
          << " " << DICTVALS(data)[idx]->toString();
    }

    if ( DICTKEYS(data).size() ) {
        s << " ";
    }

    s << "}";
}

void ExprWriter::writeNumeric(stringstream & s, const ScamData * data)
{
    if ( ScamNumeric::isNaN(data) ) {
        s << "+nan.0";
    }
    else if ( ScamNumeric::isNegInf(data) ) {
        s << "-inf.0";
    }
    else if ( ScamNumeric::isPosInf(data) ) {
        s << "+inf.0";
    }
    else if ( ScamNumeric::isInteger(data) ) {
        s << INTVAL(data);
    }
    else if ( ScamNumeric::isRational(data) ) {
        s << NUMPART(data) << "/" << DENPART(data);
    }
    else if ( ScamNumeric::isReal(data) ) {
        s << REALVAL(data);
    }
    else if ( ScamNumeric::isComplex(data) ) {
        //
        // The complexity is so that the output is in the simplest
        // form that will be read by the scanner as the same value.
        // For example, An imaginary part of "-1i" is equivalent to
        // "-i", so the latter is used for the representation.  The
        // real pa
        //
        ExprHandle r { REALPART(data) };
        ExprHandle i { IMAGPART(data) };

        if ( ! r->isInteger() || 0 != r->asInteger() ) {
            s << r->toString();
        }

        const string irepr = i->toString();
        if ( irepr == "0" ) {
            // nothing
        }
        if ( irepr == "1" ) {
            s << "+";
        }
        else if ( irepr == "-1" ) {
            s << "-";
        }
        else {
            const char lead = *irepr.c_str();
            if ( ('+' != lead) && ('-' != lead) ) {
                s << "+";
            }
            s << irepr;
        }
         s << "i";
    }
    else {
        s << "@obj<" << &data << ">";
    }
}

void ExprWriter::writeVector(stringstream & s, const ScamData * data)
{
    string sep { "" };

    s << "#(";
    for ( auto const & e : VECTOR(data) ) {
        s << sep << e->toString();
        sep = " ";
    }
    s << ")";
}
