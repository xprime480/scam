#if ! defined(LETWORKER_HPP)
#define LETWORKER_HPP 1

#include "LetBaseWorker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetParser;

    class LetWorker : public LetBaseWorker
    {
    private:
        friend class scam::MemoryManager;
	
        LetWorker(LetParser * parser,
		  Continuation * cont,
		  Env * env,
		  bool rebind);

        static LetWorker * makeInstance(LetParser * parser,
                                        Continuation * cont,
                                        Env * env,
                                        bool rebind);

    protected:
        void do_next(ExprHandle formals,
                     ExprHandle values,
                     ExprHandle forms) override;

    private:
        const bool rebind;
    };
}

#endif
