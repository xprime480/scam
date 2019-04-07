#if ! defined(SCAMFLOAT_H)
#define SCAMFLOAT_H 1

#include "expr/ScamNumeric.hpp"

#include <string>

namespace scam
{
    class ScamFloat : public ScamNumeric
    {
    private:
        friend class MemoryManager;

    protected:
        ScamFloat(double value);

    private:
        static ScamFloat * makeInstance(double value);

    public:
        std::string toString() const override;

        bool isFloat() const override;
        double toFloat() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        double const value;
    };
}

#endif
