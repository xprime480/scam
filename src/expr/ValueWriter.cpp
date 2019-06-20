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
    extern void writeError(std::stringstream & s, ScamValue data);
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
            s << (data->boolValue() ? "#t" : "#f");
            break;

        case ScamData::ByteVector:
            writeByteVector(s, data);
            break;

        case ScamData::Character:
            s << "#\\" << data->charValue();
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

        case ScamData::Keyword:
        case ScamData::Symbol:
            s << data->stringValue();
            break;

        case ScamData::String:
            s << '"' << data->stringValue() << '"';
            break;

        case ScamData::Error:
            writeError(s, data);
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
            s << "Special Form " << data->sfName();
            break;

        case ScamData::Primitive:
            s << "Primitive " << data->primName();
            break;

        case ScamData::Port:
            s << data->portValue()->describe();
            break;

        case ScamData::Eof:
            s << "eof";
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
        const ScamData::ByteVectorData & bv = data->byteVectorData();

        s << "#u8(";
        for ( auto const & e : bv ) {
            s << sep << (int)e;
            sep = " ";
        }
        s << ")";
    }

    void writeClosure(stringstream & s, ScamValue data)
    {
        s << "(";

        if ( data->closureMacroLike() ) {
            s << "macro ";
        }
        else {
            s << "lambda ";
        }
        s << writeValue(data->closureDef()->getArgs()->getValue());
        s << " ";

        const size_t count = data->closureDef()->getFormCount();
        for ( size_t idx = 0 ; idx < count ; ++ idx ) {
            if ( idx > 0 ) {
                s << " ";
            }
            s << writeValue(data->closureDef()->getForm(idx));
        }

        s << ")";
    }

    void writePair(stringstream & s, ScamValue data)
    {
        s << "(";
        s << writeValue(data->carValue());
        ScamValue next = data->cdrValue();
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
        ScamData::DictKeyData   & keys = data->dictKeys();
        ScamData::DictValueData & vals = data->dictValues();

        s << "{";

        for ( size_t idx = 0 ; idx < keys.size() ; ++idx ) {
            s << " " << writeValue(keys[idx])
              << " " << writeValue(vals[idx]);
        }

        if ( keys.size() ) {
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
            s << data->intPart();
        }
        else if ( isRational(hack) ) {
            s << data->numPart() << "/" << data->denPart();
        }
        else if ( isReal(hack) ) {
            s << data->realValue();
        }
        else if ( isComplex(hack) ) {
            //
            // The complexity is so that the output is in the simplest
            // form that will be read by the scanner as the same value.
            // For example, An imaginary part of "-1i" is equivalent to
            // "-i", so the latter is used for the representation.  The
            // real pa
            //
            ScamValue r { data->realPart() };
            ScamValue i { data->imagPart() };

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
        const ScamData::VectorData & vec = data->vectorData();

        string sep { "" };

        s << "#(";
        for ( auto const & e : vec ) {
            s << sep << writeValue(e);
            sep = " ";
        }
        s << ")";
    }

    void writeError(std::stringstream & s, ScamValue data)
    {
        vector<string> values;

        for ( const auto & v : data->errorIrritants() ) {
            auto s = writeValue(v);
            values.push_back(s);
        }

        int state = 0;
        size_t index = 0;
        const string & msg = data->errorMessage();
        size_t size = msg.size();

        for ( size_t cur = 0 ; cur < size ; ++cur  ) {
            const char c = msg[cur];
            switch ( state ) {
            case 0:
                if ( '%' == c ) {
                    state = 1;
                }
                else {
                    s << c;
                }
                break;

            case 1:             // have seen a %
                if ( '{' == c ) {
                    index = 0;
                    state = 2;
                }
                else {
                    s << '%' << c;
                    state = 0;
                }
                break;

            case 2:             // have seen %{
                if ( isdigit(c) ) {
                    index = index * 10 + (c - '0');
                    state = 3;
                }
                else {
                    state = 4;
                }
                break;

            case 3:             // have seen %{digit+
                if ( isdigit(c) ) {
                    index = index * 10 + (c - '0');
                }
                else if ( '}' == c ) { // %{digit+} is time to write value
                    if ( index >= values.size() ) {
                        s << '?';
                    }
                    else {
                        s << values[index];
                    }
                    state = 0;
                }
                else {
                    state = 4;
                }
                break;

            case 4:             // have seen %{...non-digit...
                if ( '}' == c ) {
                    state = 0;
                }
                break;

            default:
                break;
            }

        }
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

            case ScamData::Eof:
                s << "EOF ";
                break;

            default:
                s << "unknown with type" << data->type;
                break;
            }
        }
    }
}

