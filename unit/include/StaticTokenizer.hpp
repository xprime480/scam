#if !defined(STATICTOKENIZER_HPP)
#define STATICTOKENIZER_HPP 1

#include "input/Token.hpp"
#include "input/Tokenizer.hpp"

#include <vector>

namespace scam
{
    namespace test_impl
    {
        class StaticTokenizer : public scam::Tokenizer
        {
        public:
            StaticTokenizer(std::vector<scam::Token> const & tokens);

            void mark() override;
            Token next() override;

        private:
            std::vector<scam::Token> const & tokens;
            size_t index;
        };
    }
}

#endif
