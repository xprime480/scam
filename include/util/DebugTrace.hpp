#if ! defined(DEBUGTRACE_H)
#define DEBUGTRACE_H 1

#include <sstream>
#include <string>

namespace scam
{
    extern bool scamIsTracing;
    struct ScamTraceScope
    {
        ScamTraceScope();
        ~ScamTraceScope();
    };

    void scamLog(const std::string & msg);

    template <typename ... Ts>
    int scamTrace(Ts && ... args)
    {
        if ( scam::scamIsTracing && sizeof...(Ts) > 0 ) {
            std::stringstream s;
            int dummy[sizeof...(Ts)] = { (s << '<' << args << '>' << "\t", 0)... };
            s << "\n";
            scamLog(s.str());
            return dummy[0];
        }
        return 0;
    }
}

#endif
