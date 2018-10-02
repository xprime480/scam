#if ! defined(SCAMEXPR_H)
#define SCAMEXPR_H 1

#include <string>

namespace scam
{
    class ScamContext;

    class ScamExpr
    {
    public:
        virtual ~ScamExpr() {};

        virtual std::string toString() const = 0;
        virtual void eval(ScamContext & context) = 0;

        virtual bool isNull() const { return false; }
        virtual bool error() const { return false; }
        virtual bool truth() const { return true; }

        virtual bool isNumeric() const { return false; }
        virtual bool isFloat() const { return false; }
        virtual double toFloat() const { throw "Fail"; return 0.0; }
    };
}

#endif
