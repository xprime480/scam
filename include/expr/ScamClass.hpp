#if ! defined(SCAMCLASS_H)
#define SCAMCLASS_H 1

#include "expr/ScamData.hpp"

namespace scam
{
    class ClassDefParser;
    class Env;
    class ScamClassAdapter;

    class ScamClass : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamClass(ClassDefParser * def, Env * capture);
        static ScamClass * makeInstance(ClassDefParser * def, Env * capture);

    public:
        friend class ScamClassAdapter;
    };
}

#endif
