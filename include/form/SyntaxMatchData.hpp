#if ! defined(SYNTAXMATCHDATA_HPP)
#define SYNTAXMATCHDATA_HPP 1

#include "ScamFwd.hpp"

#include <map>
#include <string>
#include <vector>

namespace scam
{
    /*
     * class to store instances of variable matches for one
     * instance of template expansion
     */
    class SyntaxMatchVariable
    {
    public:
        SyntaxMatchVariable(bool ellipsis);
        void add(ScamValue value);
        ScamValue get(unsigned n) const;

    private:
        bool ellipsis;
        std::vector<ScamValue> data;
    };

    /*
     * class to store all pattern variables for one instance of
     * template expansion
     */
    class SyntaxMatchData
    {
    public:
        void add(std::string identifier, bool ellipsis, ScamValue value);
        ScamValue get(std::string identifier, unsigned n) const;

    private:
        std::map<std::string, SyntaxMatchVariable> data;
    };
}

#endif
