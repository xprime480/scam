#if ! defined(QQCONSLISTCDRCONT_HPP)
#define QQCONSLISTCDRCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class  QQConsListCdrCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        QQConsListCdrCont(ScamValue car,
                          Continuation * cont,
                          Env * env,
                          ScamEngine * engine);

        static QQConsListCdrCont * makeInstance(ScamValue car,
                                                Continuation * cont,
                                                Env * env,
                                                ScamEngine * engine);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        ScamValue car;
        Continuation * cont;
        Env  *      env;

        void handle(ScamValue expr);
        bool check_splice(ScamValue expr);
        void do_splice(ScamValue expr);
    };
}

#endif
