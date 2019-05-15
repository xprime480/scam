#if ! defined(SCAMRATIONAL_H)
#define SCAMRATIONAL_H 1

#include "expr/ScamReal.hpp"

#include <string>

namespace scam
{
    class ScamRational : public ScamReal
    {
    private:
        friend class MemoryManager;

    protected:
        ScamRational(int num, int den, bool exact, bool managed = true);

    private:
        static ScamRational *
        makeInstance(int num, int den, bool exact, bool managed = true);

    public:
        std::string toString() const override;

        bool isRational() const override;

        std::pair<int, int> toRational() const override;

    private:
        const int num;
        const int den;
    };
}

#endif
