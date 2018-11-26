#if ! defined(CONTINUATION_H)
#define CONTINUATION_H

#include <memory>
#include <string>

namespace scam
{
    class ScamExpr;

    class Continuation
    {
    public:
        Continuation(char const * name);
        virtual ~Continuation();

        virtual void run(ScamExpr * expr);
        std::string id() const;

    private:
        std::string const name;
        static std::string makeName(char const * id);
    };

    using ContHandle = std::shared_ptr<Continuation>;
}

#endif
