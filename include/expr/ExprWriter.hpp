#if ! defined(EXPRWRITER_HPP)
#define EXPRWRITER_HPP 1

#include <string>
#include <sstream>

namespace scam
{
    class ScamData;

    class ExprWriter
    {
    public:
        static std::string write(const ScamData * data);

    private:
        static void
        writeByteVector(std::stringstream & s, const ScamData * data);

        static void writeClosure(std::stringstream & s, const ScamData * data);
        static void writeCons(std::stringstream & s, const ScamData * data);
        static void writeDict(std::stringstream & s, const ScamData * data);
        static void writeNumeric(std::stringstream & s, const ScamData * data);
        static void writeVector(std::stringstream & s, const ScamData * data);
    };
}

#endif
