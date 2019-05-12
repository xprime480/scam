#if ! defined(SCAMPOSINF_H)
#define SCAMPOSINF_H 1

#include "ScamFwd.hpp"
#include "expr/ScamSpecialNumeric.hpp"

namespace scam
{
    class ScamPosInf : public ScamSpecialNumeric
    {
    private:
        friend class MemoryManager;
        ScamPosInf();
        static ScamPosInf * makeInstance();

    public:
        std::string toString() const override;
    };
}

#endif
