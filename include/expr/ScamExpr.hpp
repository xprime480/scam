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
        virtual std::string toString() const;
        virtual void eval(ScamContext & context);

        virtual bool isNull() const;
        virtual bool error() const;
    };
}

#endif
