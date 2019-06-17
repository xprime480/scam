#if ! defined(ENV_H)
#define ENV_H 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"

#include <map>
#include <string>

namespace scam
{
    struct EnvData;

    class Env : public ManagedObject
    {
    private:
        friend class scam::MemoryManager;

        Env();
        static Env * makeInstance();

    public:
        void mark() override;

        void put(ScamValue key, ScamValue val);
        bool check(ScamValue key, bool checkParent = true) const;
        ScamValue get(ScamValue key) const;

        void reset();
        Env * extend() const;
        Env * getParent() const;
        Env * getTop() const;

        void assign(ScamValue key, ScamValue val);
        void remove(ScamValue key);

        void dump(size_t max, bool full = false) const;

    private:
        std::map<std::string, ScamValue> table;
        Env * parent;
    };
}

#endif
