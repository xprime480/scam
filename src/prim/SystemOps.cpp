
#include "prim/SystemOps.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/EvalString.hpp"

#include <fstream>
#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void apply_args(ScamExpr * args,
                           ContHandle cont,
                           ScamEngine * engine);
}

Load::Load(ScamEngine * engine)
    : Primitive("load")
    , engine(engine)
{
}

void Load::applyArgs(ScamExpr * args, ContHandle cont)
{
    apply_args(args, cont, engine);
}

namespace
{
    void apply_args(ScamExpr * args, ContHandle cont, ScamEngine * engine)
    {
        string filename = args->nthcar(0)->toString();
        ifstream source;

        source.open(filename);
        if ( ! source.good() ) {
            stringstream s;
            s << "Unable to open file " << filename;
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
            return;
        }

        char buf[1024];
        stringstream text;

        while ( source.good() && ! source.eof() ) {
            source.getline(buf, sizeof ( buf ));
            text << buf << "\n";
        }

        string data = text.str();
        EvalString helper(engine, data);
        ExprHandle result = helper.getLast();
        cont->run(result.get());
    }
}


