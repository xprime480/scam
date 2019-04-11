#if ! defined(PRIMITIVE_LOAD_H)
#define PRIMITIVE_LOAD_H 1

#include "prim/Primitive.hpp"

#include <string>
#include <fstream>

namespace scam
{
    class ScamEngine;
    class ScamExpr;
    class Continuation;

    class Load : public Primitive
    {
    private:
        Load(ScamEngine * engine);

    public:
        static Load * makeInstance(ScamEngine * engine);
        void applyArgs(ScamExpr * args, Continuation * cont) override;

    private:
        ScamEngine * engine;

        bool open_file(std::ifstream & source,
                       std::string const & filename,
                       Continuation * cont);

        std::string get_data(std::ifstream & source);

        bool file_exists(std::string fullpath);

        void file_not_found(std::string const & filename,
                            Continuation * cont);

        ScamExpr * get_path();

        ScamExpr * default_path();

        ScamExpr * convert_path(char const * path);

        std::string next_element(char const *& path);

        std::string make_path(std::string dirname,
                              std::string filename);

    };
}

#endif
