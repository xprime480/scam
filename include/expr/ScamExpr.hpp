#if ! defined(SCAMEXPR_H)
#define SCAMEXPR_H 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"
#include "expr/ScamData.hpp"

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

        bool hasApply() const;

        virtual void
        apply(ExprHandle args, Continuation * cont, Env * env);

        virtual void mapEval(Continuation * cont, Env * env) const;

        bool isNull() const;
        bool error() const;
	bool truth() const;

        bool isBoolean() const;
        bool isChar() const;
        virtual char toChar() const;
        bool isString() const;
        bool isSymbol() const;
        bool isKeyword() const;

        bool isNumeric() const;
        virtual bool isExact() const;
        bool isComplex() const;
        bool isReal() const;
        bool isRational() const;
        bool isInteger() const;

        bool isNaN() const;
        bool isNegInf() const;
        bool isPosInf() const;

        virtual double asDouble() const;
        virtual std::pair<int, int> asRational() const;
        virtual int asInteger() const;

        virtual ConstExprHandle realPart() const;
        virtual ConstExprHandle imagPart() const;

        bool isNil() const;
        bool isCons() const;
        bool isList() const;
        virtual ExprHandle getCar() const;
        virtual ExprHandle getCdr() const;

        bool isVector() const;
        bool isByteVector() const;

        bool isProcedure() const;
        bool isClass() const;
        bool isInstance() const;

        bool isDict() const;

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

    protected:
        ScamData data;

    private:
        mutable Env * metadata;
    };
}

#endif
