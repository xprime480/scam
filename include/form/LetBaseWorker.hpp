#if ! defined(LETBASEWORKER_HPP)
#define LETBASEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetBaseWorker : public Worker
    {
    protected:
        LetBaseWorker(char const * name,
                      ExprHandle args,
                      Continuation * cont,
                      Env * env);

    public:
        void mark() const override;
        void run() override;

    protected:
        Continuation * cont;
        Env * env;

        virtual void
        do_next(ExprHandle formals, ExprHandle values, ExprHandle forms) = 0;

    private:
        ExprHandle args;

        void report_error();

        /**
         * verify_single
         *
         * Verify that the given expression is a list of the form
         * "(sym expr)"
         */
        bool verify_single(ExprHandle arg);

        /**
         * verify_next
         *
         * Verify that the current argument is valid.
         *
         * If so, continue checking the rest of list.
         *
         */
        bool verify_list(ExprHandle check);

        /**
         * Verify the argument list is structurally sound.
         *
         * \@args is well-formed if it a list of zero or more pairs
         * where the first item is a symbol and the second is any
         * form.
         *
         * @return true iff the args is structurally correct.
         */
        bool verify_args();

        ExprHandle parse_bindings(ExprHandle bindings);
        ExprHandle parse_args();
    };
}

#endif

