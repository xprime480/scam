#if ! defined(SCAMNAN_H)
#define SCAMNAN_H 1

#include "ScamFwd.hpp"
#include "expr/ScamSpecialNumeric.hpp"

namespace scam
{
    class ScamNaN : public ScamSpecialNumeric
    {
    private:
        friend class MemoryManager;
        ScamNaN();
        static ScamNaN * makeInstance();

    public:
        std::string toString() const override;

        bool isNaN() const override;
    };
}

#endif
