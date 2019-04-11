#if ! defined(SCAMEXPR_H)
#define SCAMEXPR_H 1

#include "util/ManagedObject.hpp"

#include <string>

namespace scam
{
    class Continuation;
    class Env;

    class ScamExpr : public ManagedObject
    {
    protected:
        ScamExpr(bool managed = true);

    public:
        void mark() const override;

        virtual std::string toString() const = 0;
        virtual void eval(Continuation * cont, Env * env);

        virtual bool hasApply() const;
        virtual void apply(ScamExpr * args, Continuation * cont, Env * env);
        virtual void mapEval(Continuation * cont, Env * env);

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
        virtual bool isFloat() const;
        virtual double toFloat() const;
        virtual bool isInteger() const;
        virtual int toInteger() const;

        virtual bool isNil() const;
        virtual bool isCons() const;
        virtual bool isList() const;
        virtual ScamExpr * getCar() const;
        virtual ScamExpr * getCdr() const;

        virtual bool isVector() const;

        virtual bool isProcedure() const;
        virtual bool isClass() const;
        virtual bool isInstance() const;

        virtual bool isDict() const;

        virtual size_t length() const;
        virtual ScamExpr * nthcar(size_t n) const;
        virtual ScamExpr * nthcdr(size_t n) const;

        virtual ScamExpr * withEnvUpdate(Env * updated) const;

        virtual void setSelf(ScamExpr * expr) const;
        virtual void setParent(ScamExpr * expr) const;

        virtual bool equals(ScamExpr const * expr) const;

        void setMeta(std::string const & key, ScamExpr * value);
        bool hasMeta(std::string const & key) const;
        ScamExpr * getMeta(std::string const & key) const;

    private:
        Env * metadata;
    };
}

#endif
