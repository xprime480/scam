#if ! defined(SCAMFLOAT_H)
#define SCAMFLOAT_H 1

#include "expr/ScamNumeric.hpp"

#include <string>

namespace scam
{
    class ScamFloat : public ScamNumeric
    {
    public:
        ScamFloat(double value);
        std::string toString() const override;
        void eval(ScamContext & context) override;

        bool isFloat() const override;
        double toFloat() const override;

    private:
        double value;
    };
}

#endif
