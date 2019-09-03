#if ! defined(MAPCDR_HPP)
#define MAPCDR_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class  MapCdr : public Worker
    {
    private:
        friend class scam::MemoryManager;

        MapCdr(ScamValue car,
               ScamValue cdr,
               Continuation * original,
               Env * env);

        static MapCdr * makeInstance(ScamValue car,
               ScamValue cdr,
               Continuation * original,
               Env * env);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      cdr;
        Continuation * cont;
        Env          * env;
    };
}

#endif
