#if ! defined(CLASSWORKER_HPP)
#define CLASSWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ScamClass;

    class ClassWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ClassWorker(const ScamClass * cls,
                    ScamValue args,
                    Continuation * cont,
                    Env * env);

        static ClassWorker * makeInstance(const ScamClass * cls,
                                          ScamValue args,
                                          Continuation * cont,
                                          Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        const ScamClass * cls;
        ScamValue        args;
        Continuation    * cont;
        Env             * env;
    };
}

#endif
