#if ! defined(SCAMCONTENTS_HPP)
#define SCAMCONTENTS_HPP 1

namespace scam
{
    template <typename T>
    void safeMark(T* val)
    {
        if ( val ) {
            val->mark();
        }
    }

    namespace ScamData__impl
    {
        struct ScamContents
        {
            virtual ~ScamContents() {}
            virtual void mark() const {}
        };

        struct BooleanContents : public ScamContents
        {
            static const constexpr char * name = "boolean";

            BooleanContents()
                : value(false)
            {
            }

            bool value;
        };

        struct CharacterContents : public ScamContents
        {
            static const constexpr char * name = "character";

            CharacterContents()
                : value(' ')
            {
            }

            unsigned char value;
        };

        struct StringContents : public ScamContents
        {
            static const constexpr char * name = "string";

            StringContents()
                : value("")
            {
            }

            std::string value;
        };

        struct ErrorContents : public ScamContents
        {
            static const constexpr char * name = "error";

            ErrorContents()
                : handled(false)
                , category(nullptr)
            {
            }

            void mark() const override
            {
                for ( auto i : irritants ) {
                    i->mark();
                }
                safeMark(category);
            }

            std::string            msg;
            std::vector<ScamValue> irritants;
            bool                   handled;
            ScamValue              category;
        };

        struct PairContents : public ScamContents
        {
            static const constexpr char * name = "pair";

            PairContents()
                : car(nullptr)
                , cdr(nullptr)
            {
            }

            void mark() const override
            {
                safeMark(car);
                safeMark(cdr);
            }

            ScamValue car;
            ScamValue cdr;
        };

        struct VectorLikeContents : public ScamContents
        {
            static const constexpr char * name = "vector-like";

            void mark() const override
            {
                for ( auto v : value ) {
                    safeMark(v);
                }
            }

            std::vector<ScamValue> value;
        };

        struct VectorContents : public VectorLikeContents
        {
            static const constexpr char * name = "vector";
        };

        struct MultipleValueContents : public VectorLikeContents
        {
            static const constexpr char * name = "multiple";
        };

        struct ByteVectorContents : public ScamContents
        {
            static const constexpr char * name = "byte vector";

            std::vector<unsigned char> value;
        };

        struct ContinuationContents : public ScamContents
        {
            static const constexpr char * name = "continuation";

            ContinuationContents()
                : value(nullptr)
            {
            }

            void mark() const override
            {
                safeMark(value);
            }

            Continuation * value;
        };

        struct PortContents : public ScamContents
        {
            static const constexpr char * name = "port";

            PortContents()
                : value(nullptr)
            {
            }

            ScamPort * value;
        };

        struct EnvContents : public ScamContents
        {
            static const constexpr char * name = "env";

            EnvContents()
                : value(nullptr)
            {
            }

            void mark() const override
            {
                safeMark(value);
            }

            Env * value;
        };

        struct SyntaxContents : public ScamContents
        {
            static const constexpr char * name = "syntax";

            void mark() const override
            {
                value.mark();
            }

            SyntaxRules value;
        };

        struct DictContents : public ScamContents
        {
            static const constexpr char * name = "dict";

            void mark() const override
            {
                for ( auto k : keys ) {
                    safeMark(k);
                }
                for ( auto v : vals ) {
                    safeMark(v);
                }
            }

            std::vector<ScamValue> keys;
            std::vector<ScamValue> vals;
        };

        struct ClosureContents : public ScamContents
        {
            static const constexpr char * name = "closure";

            void mark() const override
            {
                lambda.mark();
                safeMark(env);
            }

            LambdaDef lambda;
            Env     * env;
        };

        struct ClassContents : public ScamContents
        {
            static const constexpr char * name = "class";

            void mark() const override
            {
                def.mark();
                safeMark(capture);
            }

            ClassDef def;
            Env    * capture;
        };

        struct InstanceContents : public ScamContents
        {
            static const constexpr char * name = "instance";

            void mark() const override
            {
                safeMark(priv);
                safeMark(local);
            }

            Env * priv;
            Env * local;
        };

        struct PrimitiveContents : public ScamContents
        {
            static const constexpr char * name = "primitive";

            std::string    fname;
            PrimFunction   func;
        };

        struct SpecialFormContents : public ScamContents
        {
            static const constexpr char * name = "special form";

            std::string   fname;
            SfFunction    func;
        };

        struct PlaceholderContents : public ScamContents
        {
            static const constexpr char * name = "placeholder";

            PlaceholderContents()
                : value(nullptr)
            {
            }

            void mark() const override
            {
                safeMark(value);
            }

            ScamValue value;
        };

        struct NumericContents : public ScamContents
        {
            static const constexpr char * name = "numeric";

            NumericContents()
                : exact(true)
            {
            }

            bool exact;

        };

        struct ComplexContents : public NumericContents
        {
            static const constexpr char * name = "complex";

            ComplexContents()
                : real(nullptr)
                , imag(nullptr)
            {
            }

            void mark() const override
            {
                safeMark(real);
                safeMark(imag);
            }

            ScamValue real;
            ScamValue imag;
        };

        struct RealContents : public NumericContents
        {
            static const constexpr char * name = "real";

            RealContents()
                : value(0.0)
            {
            }

            double value;
        };

        struct RationalContents : public NumericContents
        {
            static const constexpr char * name = "rational";

            RationalContents()
                : num(0)
                , den(1)
            {
            }

            int num;
            int den;
        };

        struct IntegerContents : public NumericContents
        {
            static const constexpr char * name = "integer";

            IntegerContents()
                : value(0)
            {
            }

            int value;
        };
    }
}

#endif
