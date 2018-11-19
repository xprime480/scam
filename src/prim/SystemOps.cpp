
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
    extern ExprHandle default_path();

    string next_element(char const *& path)
    {
        stringstream s;
        while ( true ) {
            const char c = *path++;
            if ( ':' == c || ! c  ) {
                return s.str();
            }
            s << c;
        }
    }

    ExprHandle convert_path(char const * path)
    {
        ExprVec dp;

        while ( *path ) {
            string element = next_element(path);
            if ( ! element.empty() ) {
                dp.push_back(ExpressionFactory::makeString(element));
            }
        }

        if ( dp.empty() ) {
            return default_path();
        }
        return ExpressionFactory::makeVector(dp);
    }

    ExprHandle default_path()
    {
        return convert_path(".:..");
    }

    ExprHandle get_path()
    {
        ExprHandle rv = ExpressionFactory::makeNull();

        char const * path = getenv("SCAM_PATH");
        if ( ! path || ! *path ) {
            rv = default_path();
        }
        else {
            rv = convert_path(path);
        }
        return rv;
    }

    string make_path(string dirname, string filename)
    {
        stringstream s;
        s << dirname << "/" << filename;
        return s.str();
    }

    bool file_exists(string fullpath)
    {
        ifstream x;
        x.open(fullpath);
        if ( x.good() ) {
            x.close();
            return true;
        }

        return false;
    }

    void file_not_found(string const & filename, ContHandle cont)
    {
        stringstream s;
        s << "Unable to open file " << filename;
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err.get());
    }

    bool open_file(ifstream & source, string const & filename, ContHandle cont)
    {
        ExprHandle path = get_path();

        size_t n = path->length();
        for ( size_t i = 0 ; i < n ; ++i ) {
            string fullpath = make_path(path->nthcar(i)->toString(), filename);
            if ( file_exists(fullpath) ) {
                source.open(fullpath);
                return true;
            }
        }

        file_not_found(filename, cont);
        return false;
    }

    string get_data(ifstream & source)
    {
        char buf[1024];
        stringstream text;

        while ( source.good() && ! source.eof() ) {
            source.getline(buf, sizeof ( buf ));
            text << buf << "\n";
        }
        source.close();

        return text.str();
    }

    void apply_args(ScamExpr * args, ContHandle cont, ScamEngine * engine)
    {
        string filename = args->nthcar(0)->toString();
        ifstream source;

        if ( ! open_file(source, filename, cont) ) {
            return;
        }

        string data = get_data(source);

        EvalString helper(engine, data);
        ExprHandle result = helper.getLast();
        cont->run(result.get());
    }
}
