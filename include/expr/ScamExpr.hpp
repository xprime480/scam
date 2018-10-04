#if ! defined(SCAMEXPR_H)
#define SCAMEXPR_H 1

#include <memory>
#include <string>

namespace scam
{
    class ScamContext;

    class ScamExpr
    {
    public:
        virtual ~ScamExpr();

        virtual std::string toString() const = 0;
        virtual void eval(ScamContext & context);

        virtual bool hasApply() const;
        virtual void apply(std::shared_ptr<ScamExpr> const & args,
                           ScamContext const & context);

        virtual bool isNull() const;
        virtual bool error() const;
        virtual bool truth() const;

        virtual bool isBoolean() const;
        virtual bool isChar() const;
        virtual char toChar() const;
        virtual bool isString() const;
        virtual bool isSymbol() const;

        virtual bool isNumeric() const;
        virtual bool isFloat() const;
        virtual double toFloat() const;
        virtual bool isInteger() const;
        virtual int toInteger() const;

        virtual bool isNil() const;
        virtual bool isCons() const;
        virtual bool isList() const;
        virtual std::shared_ptr<ScamExpr> getCar() const;
        virtual std::shared_ptr<ScamExpr> getCdr() const;

        virtual std::shared_ptr<ScamExpr> clone() = 0;
    };
}

#endif
