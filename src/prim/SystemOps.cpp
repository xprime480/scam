
#include "prim/SystemOps.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/EvalString.hpp"

#include <cstdlib>
#include <cstring>
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
    ExprHandle default_path()
    {
        ExprVec dp;
        ExprHandle dot = ExpressionFactory::makeString(".");
        dp.push_back(dot);
        ExprHandle dotdot = ExpressionFactory::makeString("..");
        dp.push_back(dotdot);
        ExprHandle p = ExpressionFactory::makeVector(dp);
        return p;
    }

    ExprHandle get_path()
    {
        char const * path = getenv("SCAM_PATH");
        if ( ! path || ! *path ) {
            return default_path();
        }

        ExprVec dp;
        stringstream s;
        while ( true ) {
            const char c = *path++;
            if ( ':' == c || ! c  ) {
                string p = s.str();
                if ( ! p.empty() ) {
                    dp.push_back(ExpressionFactory::makeString(p));
                    s.str("");
                }
            }
            else {
                s << c;
            }

            if ( ! c ) {
                break;
            }
        }

        if ( dp.empty() ) {
            return default_path();
        }
        return ExpressionFactory::makeVector(dp);
    }

    bool open_file(ifstream & source, string const & filename, ContHandle cont)
    {
        ExprHandle path = get_path();

        size_t n = path->length();
        for ( size_t i = 0 ; i < n ; ++i ) {
            stringstream s;
            s << path->nthcar(i)->toString() << "/" << filename;
            string fullpath = s.str();
            ifstream x;
            x.open(fullpath);
            if ( x.good() ) {
                x.close();
                source.open(fullpath);
                return true;
            }
        }

        stringstream s;
        s << "Unable to open file " << filename;
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err.get());
        return false;
    }

    void apply_args(ScamExpr * args, ContHandle cont, ScamEngine * engine)
    {
        string filename = args->nthcar(0)->toString();
        ifstream source;

        if ( ! open_file(source, filename, cont) ) {
            return;
        }

        char buf[1024];
        stringstream text;

        while ( source.good() && ! source.eof() ) {
            source.getline(buf, sizeof ( buf ));
            text << buf << "\n";
        }
        source.close();

        string data = text.str();
        EvalString helper(engine, data);
        ExprHandle result = helper.getLast();
        cont->run(result.get());
    }
}


