#if ! defined(SCAM_EXCEPTION_H)
#define SCAM_EXCEPTION_H

#include <string>

namespace scam
{
    class ScamException
    {
    public:
        ScamException(std::string const & msg);

        std::string const & getMessage() const;

    private:
        std::string const msg;
    };
}

#endif
