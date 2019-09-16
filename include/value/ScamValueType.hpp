#if ! defined(SCAMVALUETYPE_HPP)
#define SCAMVALUETYPE_HPP 1

namespace scam
{
    enum class ScamValueType : unsigned char
        {
         /* atomic types */
         Nothing,
         Null,
         Boolean,
         Character,
         String,
         Symbol,
         Keyword,

         /* numeric types */
         Complex,
         Real,
         Rational,
         Integer,
         NaN,
         NegInf,
         PosInf,

         /* structured types */
         Error,
         Pair,
         Vector,
         ByteVector,
         Multiple,
         Cont,
         Port,
         Eof,
         ScamEnv,
         Syntax,
         Closure,

	 /* scam only structured types */
         Dict,
         Class,
         Instance,

         /* implmentation types */
         Primitive,
         SpecialForm,
         Placeholder
        };

    class NaNType {};
    class NegInfType {};
    class PosInfType {};
}

#endif
