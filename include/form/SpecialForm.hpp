#if ! defined(SPECIALFORM_H)
#define SPECIALFORM_H 1

#include "expr/ScamData.hpp"

namespace scam
{
    class SpecialForm : public ScamData
    {
    public:
        SpecialForm(std::string const & name,
                    SfFunction func,
                    ScamEngine * engine = nullptr,
                    bool managed = false);
    };
}

#endif
