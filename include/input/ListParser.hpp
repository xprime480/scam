#if ! defined(LISTPARSER_HPP)
#define LISTPARSER_HPP 1

#include "input/ArgParser.hpp"

#include <vector>

#include "util/DebugTrace.hpp"

namespace scam
{
    class MemoryManager;

    /**
     * Given an item parser, match a list of 0.. items.
     */
    class ListParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

    protected:
        ListParser(ArgParser * itemParser)
            : itemParser(itemParser)
        {
        }

    private:
        static ListParser * makeInstance(ArgParser * itemParser)
        {
            return new ListParser(itemParser);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();
                itemParser->mark();
                for ( const auto & item : items ) {
                    item->mark();
                }
            }
        }

        bool accept(ExprHandle expr) override
        {
            scamTrace("ListParser::accept", expr, expr->toString());

            if ( ! ArgParser::accept(expr) ) {
                return false;
            }
            scamTrace("\tAccepted by ArgParser");

            clearValue();

            if ( ! expr->isCons() && ! expr->isNil() ) {
                scamTrace("\tNeither cons nor nil, rejected");
                return false;
            }

            if ( expr->isNil() ) {
                scamTrace("\tnil, accepted");
                callback(expr);
                return true;
            }

            ExprHandle current = expr;
            while ( current->isCons() ) {
                ExprHandle item = current->getCar();
                scamTrace("\tin loop, testing", item, item->toString());
                current = current->getCdr();
                scamTrace("current", current);
                if ( ! itemParser->accept(item) ) {
                    scamTrace("\trejected item");
                    clearValue();
                    return false;
                }
                scamTrace("\taccepted");
                items.push_back(item);
            }

            if ( ! current->isNil() ) {
                scamTrace("\timproper tail, value:",
                          current, current->toString());
                if ( ! itemParser->accept(current) )  {
                    scamTrace("\t\trejected");
                    clearValue();
                    return false;
                }
                items.push_back(current);
            }

            scamTrace("accepted", size(), "items");
            callback(expr);
            return true;
        }

        void clearValue() override
        {
            ArgParser::clearValue();
            items.clear();
        }

        size_t size() const
        {
            return items.size();
        }

        ExprHandle get(size_t idx) const
        {
            if ( idx >= size() ) {
                return nullptr;
            }
            return items[idx];
        }

    private:
        ArgParser * itemParser;
        std::vector<ExprHandle> items;
    };
}

#endif
