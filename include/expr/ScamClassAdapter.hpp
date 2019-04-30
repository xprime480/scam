#if ! defined(SCAMCLASSADAPTER_H)
#define SCAMCLASSADAPTER_H 1

#include "ExprFwd.hpp"
#include "ScamFwd.hpp"

#include <cstddef>

namespace scam
{
    class FunctionDefParser;
    class ScamSymbol;

    class ScamClassAdapter
    {
    public:
        ScamClassAdapter(const ScamClass * expr);

        const ScamSymbol * getBase() const;
        size_t getVarCount() const;
        const ScamSymbol * getVar(size_t idx) const;
        size_t getMethodCount() const;
        const FunctionDefParser * getMethod(size_t idx) const;
        Env * getCapture() const;

    private:
        const ScamClass * cls;
    };
}

#endif
