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

        template <typename ... Rest>
        SequenceParser(ArgParser * parser, Rest && ... rest)
            : carParser(parser)
            , cdrParser(scam::standardMemoryManager.make<SequenceParser>(rest...))
        {
        }

        /**
         * This is NOT a copy constructor.
         * It is needed when the last element of a sequence is
         * another sequence.
         */
        explicit SequenceParser(SequenceParser * parser)
            : carParser(parser)
            , cdrParser(nullptr)
        {
        }

        explicit SequenceParser(ArgParser * parser)
            : carParser(parser)
            , cdrParser(nullptr)
        {
        }

        SequenceParser()
            : carParser(nullptr)
            , cdrParser(nullptr)
        {
        }

        template <typename ... Rest>
        static SequenceParser *
        makeInstance (ArgParser * parser, Rest && ... rest)
        {
            return new SequenceParser(parser, rest...);
        }

        static SequenceParser * makeInstance(SequenceParser * parser)
        {
            return new SequenceParser(parser);
        }

        static SequenceParser * makeInstance(ArgParser * parser)
        {
            return new SequenceParser(parser);
        }

        static SequenceParser * makeInstance()
        {
            return new SequenceParser();
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();
                if ( carParser ) {
                    carParser->mark();
                }
                if ( cdrParser ) {
                    cdrParser->mark();
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

            if ( ! ArgParser::accept(expr) ) {
                return false;
            }
            scamTrace("\tAccepted by ArgParser");

            clearValue();

            if ( expr->isCons() ) {
                if ( ! acceptCons(dynamic_cast<ScamCons *>(expr)) ) {
                    return false;
                }
            }
            else if ( expr->isNil() ) {
                if ( ! acceptNil(dynamic_cast<ScamNil *>(expr)) ) {
                    return false;
                }
            }
            else if ( ! acceptGeneral(expr) ) {
                return false;
            }

            callback(expr);
            return true;
        }

        ArgParser * get(size_t idx) const
        {
            if ( 0u == idx ) {
                return carParser;
            }
            if ( cdrParser ) {
                return cdrParser->get(idx - 1);
            }
            return nullptr;
        }

    private:
        ArgParser      * carParser;
        SequenceParser * cdrParser;

        bool acceptCons(ScamCons * expr)
        {
            scamTrace("acceptCons");

            if ( ! carParser && ! cdrParser ) {
                scamTrace("more forms than parsers");
                return false;
            }

            if ( carParser && ! cdrParser ) {
                return acceptCar(expr);
            }

            return acceptPair(expr);
        }

        bool acceptNil(ScamNil * expr)
        {
            scamTrace("acceptNil");

            if ( carParser && cdrParser ) {
                scamTrace("more parsers than forms");
                return false;
            }

            if ( carParser ) {
                return acceptExact(expr);
            }

            scamTrace("No parsers and no forms");
            return true;
        }

        bool acceptGeneral(ExprHandle expr)
        {
            scamTrace("acceptGeneral");
            if ( carParser && cdrParser ) {
                scamTrace("More parsers than forms");
                return false;
            }

            if ( ! carParser ) {
                scamTrace("More forms than parsers");
                return false;
            }

            return acceptExact(expr);
        }

        bool acceptPair(ExprHandle expr)
        {
            scamTrace("acceptPair", expr->toString());

            ExprHandle car = expr->getCar();
            if ( ! carParser->accept(car) ) {
                scamTrace("Failed to accept car" );
                return false;
            }

            ExprHandle cdr = expr->getCdr();
            if ( ! cdrParser->accept(cdr) ) {
                scamTrace("Failed to accept cdr" );
                return false;
            }

            return true;
        }

        bool acceptCar(ExprHandle expr)
        {
            scamTrace("acceptCar", expr->toString(), carParser);

            ExprHandle cdr = expr->getCdr();
            scamTrace("cdr = ",  cdr, cdr->toString());

            if ( cdr->isNil() ) {
                expr = expr->getCar();
            }

            scamTrace("final car expression", expr, expr->toString());

            if ( carParser->accept(expr) ) {
                scamTrace("Tail forms accepted to accept last parser" );
                return true;
            }

            scamTrace("Last form(s) failed to accept last parser" );
            return false;
        }

        bool acceptExact(ExprHandle expr)
        {
            scamTrace("acceptExact", expr->toString(), carParser);

            if ( ! carParser->accept(expr) ) {
                scamTrace("Failed on exact accept" );
                return false;
            }

            scamTrace("Succeeded on exact accept" );
            return true;
        }
    };
}

#endif
