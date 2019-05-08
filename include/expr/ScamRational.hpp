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
        ScamRational(int num, int den);

    private:
        static ScamRational * makeInstance(int num, int den);

    public:
        std::string toString() const override;

        bool isRational() const override;

    private:
        const int num;
        const int den;
    };
}

#endif
