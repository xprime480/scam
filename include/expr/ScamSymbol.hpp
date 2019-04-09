#if ! defined(SCAMSYMBOL_H)
#define SCAMSYMBOL_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamSymbol : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamSymbol(std::string const & value, bool managed = true);
        static ScamSymbol * makeInstance(std::string const & value,
					 bool managed = true);

    public:
        std::string toString() const override;
        void eval(Continuation * cont, Env env) override;

        bool isSymbol() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        std::string const value;
    };
}

#endif
