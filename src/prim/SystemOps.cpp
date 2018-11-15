
#include "prim/SystemOps.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <fstream>
#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void apply_args(ScamExpr * args, ContHandle cont);
}

Load::Load()
    : Primitive("load")
{
}

void Load::applyArgs(ScamExpr * args, ContHandle cont)
{
    apply_args(args, cont);
}

namespace
{

    void apply_args(ScamExpr * args, ContHandle cont)
    {
        cerr << "Load::applyArgs: " << args->toString() << "\n";
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
            text << buf;
        }

	ExprHandle data = ExpressionFactory::makeString(text.str());
        cont->run(data.get());
    }
}


