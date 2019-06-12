#include "expr/ValueWriter.hpp"

#include "expr/ScamData.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "input/LambdaParser.hpp"
#include "input/ParameterListParser.hpp"
#include "port/ScamPort.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void writeByteVector(std::stringstream & s, ScamValue data);
    extern void writeClosure(std::stringstream & s, ScamValue data);
    extern void writePair(std::stringstream & s, ScamValue data);
    extern void writeDict(std::stringstream & s, ScamValue data);
    extern void writeNumeric(std::stringstream & s, ScamValue data);
    extern void writeVector(std::stringstream & s, ScamValue data);
    extern void writeType(std::stringstream & s, ScamValue data);
}

string scam::writeValue(ScamValue data)
{
    stringstream s;

    if ( isNumeric(data) ) {
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

        case ScamData::Pair:
            writePair(s, data);
            break;

        case ScamData::Dict:
            writeDict(s, data);
            break;

        case ScamData::Cont:
            s << "continuation";
            break;

        case ScamData::Error:
        case ScamData::Keyword:
        case ScamData::Symbol:
            s << STRVAL(data);
            break;

        case ScamData::String:
            s << '"' << STRVAL(data) << '"';
            break;


        case ScamData::Instance:
            s << "instance";
            break;

        case ScamData::Null:
            s << "()";
            break;

        case ScamData::Nothing:
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

        case ScamData::Port:
            s << PORT(data)->describe();
            break;

        default:
            s << "don't know how to represent this object, type = "
              << data->type;
            break;
        }
    }

    auto rv = s.str();
    return rv;
}

string scam::debugWriteValue(ScamValue data)
{
    stringstream s;

    s << "type = <";
    writeType(s, data);
    s << ">; value = <"
      << writeValue(data)
      << ">";

    return s.str() ;
}

namespace
{
    void writeByteVector(stringstream & s, ScamValue data)
    {
        string sep { "" };

        s << "#u8(";
        for ( auto const & e : BYTEVECTOR(data) ) {
            s << sep << (int)e;
            sep = " ";
        }
        s << ")";
    }

    void writeClosure(stringstream & s, ScamValue data)
    {
        s << "(";

        if ( MACROLIKE(data) ) {
            s << "macro ";
        }
        else {
            s << "lambda ";
        }
        s << writeValue(CLOSUREDEF(data)->getArgs()->getValue());
        s << " ";

        const size_t count = CLOSUREDEF(data)->getFormCount();
        for ( size_t idx = 0 ; idx < count ; ++ idx ) {
            if ( idx > 0 ) {
                s << " ";
            }
            s << writeValue(CLOSUREDEF(data)->getForm(idx));
        }

        s << ")";
    }

    void writePair(stringstream & s, ScamValue data)
    {
        s << "(";
        s << writeValue(CAR(data));
        ScamValue next = CDR(data);
        while ( ! isNull(next) ) {
            if ( isPair(next) ) {
                s << " " << writeValue(getCar(next));
                next = getCdr(next);
            }
            else {
                s << " . " << writeValue(next);
                break;
            }
        }
        s << ")";
    }

    void writeDict(stringstream & s, ScamValue data)
    {
        s << "{";

        for ( size_t idx = 0 ; idx < DICTKEYS(data).size() ; ++idx ) {
            s << " " << writeValue(DICTKEYS(data)[idx])
              << " " << writeValue(DICTVALS(data)[idx]);
        }

        if ( DICTKEYS(data).size() ) {
            s << " ";
        }

        s << "}";
    }

    void writeNumeric(stringstream & s, ScamValue data)
    {
        ScamValue hack = const_cast<ScamValue>(data);
        if ( isNaN(hack) ) {
            s << "+nan.0";
        }
        else if ( isNegInf(hack) ) {
            s << "-inf.0";
        }
        else if ( isPosInf(hack) ) {
            s << "+inf.0";
        }
        else if ( isInteger(hack) ) {
            s << INTVAL(data);
        }
        else if ( isRational(hack) ) {
            s << NUMPART(data) << "/" << DENPART(data);
        }
        else if ( isReal(hack) ) {
            s << REALVAL(data);
        }
        else if ( isComplex(hack) ) {
            //
            // The complexity is so that the output is in the simplest
            // form that will be read by the scanner as the same value.
            // For example, An imaginary part of "-1i" is equivalent to
            // "-i", so the latter is used for the representation.  The
            // real pa
            //
            ScamValue r { REALPART(data) };
            ScamValue i { IMAGPART(data) };

            if ( ! isInteger(r) || 0 != asInteger(r) ) {
                s << writeValue(r);
            }

            const string irepr = writeValue(i);
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

    void writeVector(stringstream & s, ScamValue data)
    {
        string sep { "" };

        s << "#(";
        for ( auto const & e : VECTOR(data) ) {
            s << sep << writeValue(e);
            sep = " ";
        }
        s << ")";
    }

    void writeType(std::stringstream & s, ScamValue data)
    {
        if ( isNumeric(data) ) {
            if ( isExact(data) ) {
                s << "exact ";
            }
            else {
                s << "inexact ";
            }

            if ( isNaN(data) ) {
                s << "NaN";
            }
            else if ( isNegInf(data) ) {
                s << "-inf";
            }
            else if ( isPosInf(data) ) {
                s << "+inf";
            }
            else if ( isInteger(data) ) {
                s << "integer";
            }
            else if ( isRational(data) ) {
                s << "rational";
            }
            else if ( isReal(data) ) {
                s << "real";
            }
            else if ( isComplex(data) ) {
                s << "complex";
            }
            else {
                s << "unknown numeric";
            }
        }
        else {
            switch ( data->type ) {
            case ScamData::Boolean:
                s << "boolean";
                break;

            case ScamData::ByteVector:
                s << "byte vector";
                break;

            case ScamData::Character:
                s << "character";
                break;

            case ScamData::Class:
                s << "class";
                break;

            case ScamData::Closure:
                s << "closure";
                break;

            case ScamData::Pair:
                s << "pair";
                break;

            case ScamData::Dict:
                s << "dict";
                break;

            case ScamData::Cont:
                s << "continuation";
                break;

            case ScamData::Error:
                s << "error";
                break;

            case ScamData::Keyword:
                s << "keyword";
                break;

            case ScamData::String:
                s << "string";
                break;

            case ScamData::Symbol:
                s << "symbol";
                break;

            case ScamData::Instance:
                s << "instance";
                break;

            case ScamData::Null:
                s << "nil";
                break;

            case ScamData::Nothing:
                s << "null";
                break;

            case ScamData::Vector:
                s << "vector";
                break;

            case ScamData::SpecialForm:
                s << "special form ";
                break;

            case ScamData::Primitive:
                s << "Primitive ";
                break;

            case ScamData::Port:
                s << "Port ";
                break;

            default:
                s << "unknown with type" << data->type;
                break;
            }
        }
    }
}

