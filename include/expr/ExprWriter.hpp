#if ! defined(EXPRWRITER_HPP)
#define EXPRWRITER_HPP 1

#include <string>

namespace scam
{
    class ScamData;

    class ExprWriter
    {
    public:
        static std::string write(const ScamData * data);

    private:
    };
}

#endif
