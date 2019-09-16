#include "value/ValueWriter.hpp"

#include "expr/SequenceOps.hpp"
#include "form/SyntaxRules.hpp"
#include "port/ScamPort.hpp"
#include "value/ScamData.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void writeByteVector(std::stringstream & s, ScamValue data);
    extern void writeClosure(std::stringstream & s, ScamValue data);
    extern void writePair(std::stringstream & s, ScamValue data);
    extern void writeList(stringstream & s, ScamValue data);

    extern void writeSexp(stringstream & s,
                          ScamValue data,
                          vector<ScamValue> & shared);

    extern void writeSexpHelper(stringstream & s,
                                ScamValue data,
                                map<ScamValue, size_t> & indexes,
                                set<ScamValue> & seen);

    extern void writeDict(std::stringstream & s, ScamValue data);
    extern void writeNumeric(std::stringstream & s, ScamValue data);
    extern void writeVector(std::stringstream & s, ScamValue data);
    extern void writeError(std::stringstream & s, ScamValue data);
}

string scam::writeValue(ScamValue data)
{
    stringstream s;

    if ( isNumeric(data) ) {
        writeNumeric(s, data);
    }
    else {
        switch ( data->type ) {
        case ScamValueType::Boolean:
            s << (data->boolValue() ? "#t" : "#f");
            break;

        case ScamValueType::ByteVector:
            writeByteVector(s, data);
            break;

        case ScamValueType::Character: {
            s << "#\\";
            const char c = data->charValue();
            switch (c) {
            case 0x07:
                s << "alarm";
                break;
            case 0x08:
                s << "backspace";
                break;
            case 0x7f:
                s << "delete";
                break;
            case 0x1b:
                s << "escape";
                break;
            case 0x0a:
                s << "newline";
                break;
            case 0x00:
                s << "null";
                break;
            case 0x0d:
                s << "return";
                break;
            case 0x20:
                s << "space";
                break;
            case 0x09:
                s << "tab";
                break;
            default:
                if ( isprint(c) ) {
                    s << c;
                }
                else {
                    s << "x"
                      << "0123456789abcdef"[(c & 0xf0) >> 4]
                      << "0123456789abcdef"[(c & 0x0f)];
                }
                break;
            }
        }
            break;

        case ScamValueType::Class:
            s << "class";
            break;

        case ScamValueType::Closure:
            writeClosure(s, data);
            break;

        case ScamValueType::Pair:
            writePair(s, data);
            break;

        case ScamValueType::Dict:
            writeDict(s, data);
            break;

        case ScamValueType::Cont:
            s << "continuation";
            break;

        case ScamValueType::Keyword:
        case ScamValueType::Symbol:
            s << data->stringValue();
            break;

        case ScamValueType::String:
            s << '"' << data->stringValue() << '"';
            break;

        case ScamValueType::Error:
            writeError(s, data);
            break;

        case ScamValueType::Instance:
            s << "instance";
            break;

        case ScamValueType::Null:
            s << "()";
            break;

        case ScamValueType::Nothing:
            s << "null";
            break;

        case ScamValueType::Vector:
            writeVector(s, data);
            break;

        case ScamValueType::SpecialForm:
            s << "Special Form " << data->sfName();
            break;

        case ScamValueType::Primitive:
            s << "Primitive " << data->primName();
            break;

        case ScamValueType::Port:
            s << data->portValue()->describe();
            break;

        case ScamValueType::Eof:
            s << "eof";
            break;

        case ScamValueType::Syntax:
            s << "syntax for " << writeValue(data->syntaxRules().getName());
            break;

        case ScamValueType::ScamEnv:
            if ( isForwarder(data) ) {
                s << "forwarder";
            }
            else {
                s << "env";
            }
            break;

        case ScamValueType::Placeholder:
            s << "placeholder:" << writeValue(data->placeholderValue());
            break;

        case ScamValueType::Multiple:
            do {
                vector<ScamValue> & values = data->multipleValues();
                string sep = "";
                s << "multiple values: [";
                for ( const auto v : values ) {
                    s << sep << writeValue(v);
                    sep = "; ";
                }
                s << "]";
            } while ( false );
            break;

        default:
            s << "don't know how to represent this object, type = "
              << (unsigned char)(data->type);
            break;
        }
    }

    auto rv = s.str();
    return rv;
}

string scam::debugWriteValue(ScamValue data)
{
    stringstream s;

    s << "type = <" << describe(data->type) << ">; ";
    s << "value = <" << writeValue(data) << ">";

    return s.str() ;
}

string scam::describe(ScamValueType type)
{
    const char * text = "unknown";

    switch ( type ) {

    case ScamValueType::Complex:
        text = "complex";
        break;

    case ScamValueType::Real:
        text = "real";
        break;

    case ScamValueType::Rational:
        text = "rational";
        break;

    case ScamValueType::Integer:
        text = "integer";
        break;

    case ScamValueType::NaN:
        text = "NaN";
        break;

    case ScamValueType::NegInf:
        text = "-inf";
        break;

    case ScamValueType::PosInf:
        text = "+inf";
        break;

    case ScamValueType::Nothing:
        text = "nothing";
        break;

    case ScamValueType::Null:
        text = "null";
        break;

    case ScamValueType::Boolean:
        text = "boolean";
        break;

    case ScamValueType::Character:
        text = "character";
        break;

    case ScamValueType::Symbol:
        text = "symbol";
        break;

    case ScamValueType::Keyword:
        text = "keyword";
        break;

    case ScamValueType::String:
        text = "string";
        break;

    case ScamValueType::Error:
        text = "error";
        break;

    case ScamValueType::Pair:
        text = "pair";
        break;

    case ScamValueType::Vector:
        text = "vector";
        break;

    case ScamValueType::ByteVector:
        text = "byte vector";
        break;

    case ScamValueType::Dict:
        text = "dict";
        break;

    case ScamValueType::Closure:
        text = "closure";
        break;

    case ScamValueType::Class:
        text = "class";
        break;

    case ScamValueType::Instance:
        text = "instance";
        break;

    case ScamValueType::Cont:
        text = "continuation";
        break;

    case ScamValueType::Primitive:
        text = "primitive";
        break;

    case ScamValueType::SpecialForm:
        text = "special form";
        break;

    case ScamValueType::Port:
        text = "port";
        break;

    case ScamValueType::Eof:
        text = "eof";
        break;

    case ScamValueType::Syntax:
        text = "syntax";
        break;

    case ScamValueType::ScamEnv:
        text = "env";
        break;

    case ScamValueType::Placeholder:
        text = "placeholder";
        break;

    case ScamValueType::Multiple:
        text = "multiple values";
        break;

    default:
        break;
    }

    return text;
}

namespace
{
    void writeByteVector(stringstream & s, ScamValue data)
    {
        string sep { "" };
        const vector<unsigned char> & bv = data->byteVectorData();

        s << "#u8(";
        for ( auto const & e : bv ) {
            s << sep << (int)e;
            sep = " ";
        }
        s << ")";
    }

    void writeClosure(stringstream & s, ScamValue data)
    {
        s << "(lambda ";
        const LambdaDef & lambda = data->closureDef();
        if ( isNothing(lambda.rest) && isNull(lambda.formals)) {
            s << "()";
        }
        else if ( isNothing(lambda.rest) ) {
            s << writeValue(lambda.formals);
        }
        else if ( isNull(lambda.formals) ) {
            s << writeValue(lambda.rest);
        }
        else {
            s << "(";
            ScamValue t = lambda.formals;
            while ( ! isNull(t) ) {
                s << writeValue(getCar(t)) << " ";
                t = getCdr(t);
            }
            s << ". " << writeValue(lambda.rest) << ")";
        }
        s << " ";

        const size_t count = length(data->closureDef().forms);
        for ( size_t idx = 0 ; idx < count ; ++ idx ) {
            if ( idx > 0 ) {
                s << " ";
            }
            s << writeValue(nthcar(data->closureDef().forms, idx));
        }

        s << ")";
    }

    void writePair(stringstream & s, ScamValue data)
    {
        vector<ScamValue> shared = detectSharedStructure(data, true);

        if ( shared.empty() ) {
            writeList(s, data);
        }
        else {
            writeSexp(s, data, shared);
        }
    }

    void writeList(stringstream & s, ScamValue data)
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

    void writeSexp(stringstream & s, ScamValue data, vector<ScamValue> & shared)
    {
        set<ScamValue> seen;

        map<ScamValue, size_t> indexes;
        size_t i { 0 };
        for ( auto p : shared ) {
            indexes[p] = i++;
        }

        writeSexpHelper(s, data, indexes, seen);
    }

    void writeSexpHelper(stringstream & s,
                         ScamValue data,
                         map<ScamValue, size_t> & indexes,
                         set<ScamValue> & seen)
    {
        if ( ! isPair(data) ) {
            s << writeValue(data);
            return;
        }

        const auto iIndex = indexes.find(data);
        if ( iIndex != indexes.end() ) {
            const auto iSeen = seen.find(data);
            unsigned idx = iIndex->second;
            if ( iSeen == seen.end() ) {
                s << "#" << idx << "=";
                seen.insert(data);
            }
            else {
                s << "#" << idx << "#";
                return;
            }
        }

        s << "(";
        writeSexpHelper(s, data->carValue(), indexes, seen);

        ScamValue next = data->cdrValue();
        while ( ! isNull(next) ) {
            const auto iter = indexes.find(next);
            if ( iter != indexes.end() ) {
                s << " . #" << iter->second << "#";
                next = makeNull();
            }
            else if ( isPair(next) ) {
                s << " ";
                writeSexpHelper(s, getCar(next), indexes, seen);
                next = getCdr(next);
            }
            else {
                s << " . ";
                writeSexpHelper(s, next, indexes, seen);
                next = makeNull();
            }
        }

        s << ")";
    }

    void writeDict(stringstream & s, ScamValue data)
    {
        vector<ScamValue> & keys = data->dictKeys();
        vector<ScamValue> & vals = data->dictValues();

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
        const vector<ScamValue> & vec = data->vectorData();

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
}
