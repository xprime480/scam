#if ! defined(ENV_H)
#define ENV_H 1

#include "util/ManagedObject.hpp"

#include <map>
#include <string>

namespace scam
{
    class ScamExpr;
    struct EnvData;
    class MemoryManager;

    class Env : public ManagedObject
    {
    private:
        friend class scam::MemoryManager;

        Env();
        static Env * makeInstance();

    public:
        void mark() const override;

        void put(ScamExpr const * key, ScamExpr * val);
        bool check(ScamExpr const * key, bool checkParent = true) const;
        ScamExpr * get(ScamExpr const * key) const;

        void reset();
        Env * extend() const;
        Env * getParent() const;
        Env * getTop() const;

        void assign(ScamExpr const * key, ScamExpr * val);
        void remove(ScamExpr const * key);

        void dump(size_t max, bool full = false) const;

    private:
        std::map<std::string, ScamExpr *> table;
        Env * parent;
    };
}

#endif
