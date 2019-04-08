#if ! defined(SCAMTRACE_H)
#define SCAMTRACE_H 1

#include <iostream>

namespace scam
{
    extern bool scamIsTracing;
    struct ScamTraceScope
    {
        ScamTraceScope();
        ~ScamTraceScope();
    };

    template <typename ... Ts>
    int scamTrace(Ts && ... args)
    {
        if ( scam::scamIsTracing && sizeof...(Ts) > 0 ) {
          int dummy[sizeof...(Ts)] = { (std::cerr << args << "\t", 0)... };
          std::cerr << "\n";
          return dummy[0];
        }
        return 0;
    }
}

#endif
