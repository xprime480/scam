#if ! defined(COMMONERROR_HPP)
#define COMMONERROR_HPP 1

namespace scam
{
    class ScamExpr;
    extern ScamExpr * make_common_error(const char * text);
}

#endif
