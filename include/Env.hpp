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
        void mark() const override;

        void put(ScamEnvKeyType key, ScamValue val);
        bool check(ScamEnvKeyType key, bool checkParent = true) const;
        ScamValue get(ScamEnvKeyType key) const;

        void reset();
        Env * extend() const;
        Env * getParent() const;
        Env * getTop() const;

        void assign(ScamEnvKeyType key, ScamValue val);
        void remove(ScamEnvKeyType key);

        void dump(size_t max, bool full = false) const;

    private:
        std::map<std::string, ScamValue> table;
        Env * parent;
    };
}

#endif
