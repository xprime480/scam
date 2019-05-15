#if ! defined(SCAMEXPR_H)
#define SCAMEXPR_H 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"

#include <string>

namespace scam
{
    class ScamExpr: public ManagedObject
    {
    protected:
        ScamExpr(bool managed = true);

    public:
        void mark() const override;

        virtual std::string toString() const = 0;
        virtual void eval(Continuation * cont, Env * env) const;

        virtual bool hasApply() const;

        virtual void
        apply(ExprHandle args, Continuation * cont, Env * env);

        virtual void mapEval(Continuation * cont, Env * env) const;

        virtual bool isNull() const;
        virtual bool error() const;
        virtual bool truth() const;

        virtual bool isBoolean() const;
        virtual bool isChar() const;
        virtual char toChar() const;
        virtual bool isString() const;
        virtual bool isSymbol() const;
        virtual bool isKeyword() const;

        virtual bool isNumeric() const;
        virtual bool isExact() const;
        virtual bool isComplex() const;
        virtual bool isReal() const;
        virtual bool isRational() const;
        virtual bool isInteger() const;

        virtual bool isNaN() const;
        virtual bool isNegInf() const;
        virtual bool isPosInf() const;

        virtual double toReal() const;
        virtual std::pair<int, int> toRational() const;
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

        virtual bool isDict() const;

        virtual size_t length() const;
        virtual ExprHandle nthcar(size_t n) const;
        virtual ExprHandle nthcdr(size_t n) const;

        virtual ExprHandle withEnvUpdate(Env * updated) const;

        virtual void setSelf(ExprHandle expr) const;
        virtual void setParent(ExprHandle expr) const;

        virtual bool equals(ConstExprHandle expr) const;

        void setMeta(std::string const & key, ExprHandle value) const;
        bool hasMeta(std::string const & key) const;
        ExprHandle getMeta(std::string const & key) const;

    private:
        mutable Env * metadata;
    };
}

#endif
