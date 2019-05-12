#if ! defined(SCAMNEGINF_H)
#define SCAMNEGINF_H 1

#include "ScamFwd.hpp"
#include "expr/ScamSpecialNumeric.hpp"

namespace scam
{
    class ScamNegInf : public ScamSpecialNumeric
    {
    private:
        friend class MemoryManager;
        ScamNegInf();
        static ScamNegInf * makeInstance();

    public:
        std::string toString() const override;
    };
}

#endif
