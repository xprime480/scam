#if ! defined(SEQUENCEPARSER_HPP)
#define SEQUENCEPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "expr/ScamExpr.hpp"
#include "expr/ScamCons.hpp"
#include "expr/ScamNil.hpp"
#include "util/MemoryManager.hpp"

#include "util/DebugTrace.hpp"

namespace scam
{
    class MemoryManager;

    /**
     * Match a sequence of parsers of arbitrary type.
     */
    class SequenceParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

        template <typename ... Parsers>
        SequenceParser(Parsers && ... parsers)
        {
            saveParsers(parsers...);
        }

        template <typename ... Parsers>
        static SequenceParser *
        makeInstance (Parsers && ... parsers)
        {
            return new SequenceParser(parsers...);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();
                for ( auto p : parsers ) {
                    p->mark();
                }
            }
        }

        bool accept(ExprHandle expr) override
        {
            scamTrace("SequenceParser::accept", expr, expr->toString());

            if ( ! ArgParser::accept(expr) ) {
                return false;
            }
            scamTrace("\tAccepted by ArgParser");

            clearValue();

            ArgParser * any = standardMemoryManager.make<ArgParser>();
            ListParser * temp = standardMemoryManager.make<ListParser>(any);

            if ( ! temp->accept(expr) ) {
                scamTrace("\tNot a list of forms");
                return false;
            }

            if ( temp->size() != parsers.size() ) {
                scamTrace("\t", temp->size(), "forms for ",
                          parsers.size(), "parsers");
                return false;
            }

            for ( size_t idx = 0 ; idx < parsers.size() ; ++idx ) {
                ArgParser * p = parsers[idx];
                ExprHandle  e = temp->get(idx);
                if ( ! p->accept(e) ) {
                    scamTrace("\tSubform", idx, "rejected");
                    return false;
                }
            }

            scamTrace("accepted form");
            callback(expr);
            return true;
        }

        ArgParser * get(size_t idx) const
        {
            if ( idx < parsers.size() ) {
                return parsers[idx];
            }
            return nullptr;
        }

    private:
        std::vector<ArgParser *> parsers;

        template <typename ... Parsers>
        void saveParsers(ArgParser * parser, Parsers && ... rest)
        {
            parsers.push_back(parser);
            saveParsers(rest...);
        }

        void saveParsers()
        {
        }
    };
}

#endif
