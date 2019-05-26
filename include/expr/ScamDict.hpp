#if ! defined(SCAMDICT_H)
#define SCAMDICT_H 1

#include "expr/ScamData.hpp"

#include <vector>

namespace scam
{
    using KeyVec = std::vector<ScamValue>;
    using ValVec = std::vector<ScamValue>;

    class ScamDict : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamDict();
        ScamDict(ValVec const & args);
        static ScamDict * makeInstance();
        static ScamDict * makeInstance(ValVec const & args);

    public:
        bool has(ScamValue key) const;
        ScamValue get(ScamValue key) const;
        ScamValue put(ScamValue key, ScamValue val);
        ScamValue remove(ScamValue key);
        KeyVec const & getKeys() const;
    };
}

#endif
