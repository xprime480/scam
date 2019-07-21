#if ! defined(PATTERNDATA_HPP)
#define PATTERNDATA_HPP 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"

#include <string>
#include <vector>

namespace scam
{
    class SyntaxMatchData;

    struct PatternData : public ManagedObject
    {
        virtual ~PatternData();
        virtual bool match(ScamValue arg, SyntaxMatchData & data) = 0;
    };

    struct PatternDataNothing : public PatternData
    {
    private:
        friend class scam::MemoryManager;

        static PatternDataNothing * makeInstance();

    public:
        bool match(ScamValue arg, SyntaxMatchData & data) override;
    };

    struct PatternDataIdentifier : public PatternData
    {
    private:
        friend class scam::MemoryManager;
        PatternDataIdentifier(ScamValue identifier, bool rest);

        static PatternDataIdentifier *
        makeInstance(ScamValue identifier, bool rest = false);

    public:
        bool match(ScamValue arg, SyntaxMatchData & data) override;
        bool isRest() const;

    private:
        std::string identifier;
        bool        rest;
    };

    struct PatternDataSequence : public PatternData
    {
    private:
        friend class scam::MemoryManager;
        PatternDataSequence(const std::vector<PatternData *> & patterns);

        static PatternDataSequence *
        makeInstance(const std::vector<PatternData *> & patterns);

    public:
        bool match(ScamValue arg, SyntaxMatchData & data) override;

    private:
        std::vector<PatternData *> patterns;
    };

    struct PatternDataLiteral : public PatternData
    {
    private:
        friend class scam::MemoryManager;
        PatternDataLiteral(ScamValue value);
        static PatternDataLiteral * makeInstance(ScamValue value);

    public:
        void mark() override;
        bool match(ScamValue arg, SyntaxMatchData & data) override;

    private:
        ScamValue value;
    };
}

#endif
