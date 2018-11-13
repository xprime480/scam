#if ! defined(SCAMEXPR_H)
#define SCAMEXPR_H 1

#include "Env.hpp"

#include <memory>
#include <string>

namespace scam
{
    class Continuation;
    using ContHandle = std::shared_ptr<Continuation> ;

    class ScamExpr;
    using ExprHandle = std::shared_ptr<ScamExpr>;

    class ExpressionFactory;

    class ScamExpr
    {
    public:
        virtual ~ScamExpr();

        virtual std::string toString() const = 0;
        virtual void eval(ContHandle cont, Env env);

        virtual bool hasApply() const;
        virtual void apply(ScamExpr * args, ContHandle cont, Env env);
        virtual void mapEval(ContHandle cont, Env env);

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
        virtual ExprHandle getCar() const;
        virtual ExprHandle getCdr() const;

        virtual bool isVector() const;

        virtual bool isProcedure() const;
        virtual bool isClass() const;
        virtual bool isInstance() const;

        virtual size_t length() const;
        virtual ExprHandle nthcar(size_t n) const;
        virtual ExprHandle nthcdr(size_t n) const;

        virtual ExprHandle withEnvUpdate(Env updated) const;

        virtual void setSelf(ScamExpr * expr) const;
        virtual void setParent(ScamExpr * expr) const;

        ExprHandle clone() const;

    private:
        friend class ExpressionFactory;
        unsigned handle;
        void setHandle(unsigned h) { handle = h; }
    };
}

#endif
