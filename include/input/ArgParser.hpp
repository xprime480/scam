#if ! defined(ARGPARSER_HPP)
#define ARGPARSER_HPP 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ArgParser : public ManagedObject
    {
    private:
        friend class scam::MemoryManager;

    protected:
        ArgParser();

    private:
        static ArgParser * makeInstance();

    public:
        virtual ~ArgParser();
        void mark() const override;

        virtual bool accept(ExprHandle expr);
        virtual void callback(ExprHandle expr);
        ExprHandle getValue() const;

    protected:
        virtual void clearValue();

    private:
        ExprHandle value;
    };
}

#endif
