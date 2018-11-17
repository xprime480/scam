#if ! defined(EVALSTRING_H)
#define EVALSTRING_H 1

#include "Extractor.hpp"
#include "ScamEngine.hpp"
#include "input/StringTokenizer.hpp"

#include <string>
#include <vector>

namespace scam
{
    class EvalString
    {
    public:
        EvalString(ScamEngine * engine, std::string const & text);
        ~EvalString();

        ExprHandle getLast();
        void getAll(std::vector<ExprHandle> & exprs,  bool stopOnError);

    private:
        ScamEngine * engine;
        StringTokenizer tokenizer;
        std::shared_ptr<Extractor> cont;

        ExprHandle getNext();
    };
}

#endif
