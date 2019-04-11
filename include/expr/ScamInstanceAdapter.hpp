#if ! defined(SCAMINSTANCEADAPTER_H)
#define SCAMINSTANCEADAPTER_H 1

namespace scam
{
    class ScamExpr;
    class ScamInstance;
    class Env;

    class ScamInstanceAdapter
    {
    public:
        ScamInstanceAdapter(ScamExpr const * expr);

        Env * getFunctionMap() const;
        Env * getEnv() const;
        ScamExpr * getParent() const;

    private:
        ScamInstance const * instance;
    };
}

#endif
