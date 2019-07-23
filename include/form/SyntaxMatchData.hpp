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
        void append(const SyntaxMatchVariable & newData);

        bool isEllipsis() const;
        int count() const;

        ScamValue get(unsigned n) const;

        std::string identify() const;

    private:
        bool ellipsis;
        std::vector<ScamValue> data;

        void overflow();
    };

    /*
     * class to store all pattern variables for one instance of
     * template expansion
     */
    class SyntaxMatchData
    {
    public:
        void add(std::string identifier, bool ellipsis, ScamValue value);
        void append(const SyntaxMatchData & newData);

        bool hasEllipsisId(const std::string & identifier) const;
        int count(const std::string & identifier) const;

        ScamValue get(std::string identifier, unsigned n) const;

        std::string identify() const;

    private:
        std::map<std::string, SyntaxMatchVariable> data;
    };
}

#endif
