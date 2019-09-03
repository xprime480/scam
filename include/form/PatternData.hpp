#if ! defined(PATTERNDATA_HPP)
#define PATTERNDATA_HPP 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"

#include <set>
#include <string>
#include <vector>

namespace scam
{
    class PatternDataIdentifier;
    class SyntaxMatchData;

    using PatIDSet = std::set<PatternDataIdentifier *>;

    /*
     * Base class for describing syntax patterns
     */
    class PatternData : public ManagedObject
    {
    protected:
        PatternData();

    public:
        virtual ~PatternData();
        virtual bool match(ScamValue arg, SyntaxMatchData & data) = 0;

        virtual void tagAsEllipsis();
        bool isEllipsis() const;

        virtual void getPatternIds(PatIDSet & patternIds);

        virtual std::string identify() const = 0;

    private:
        bool ellipsis;
    };

    /*
     * Class for internal pattern data which matches nothing.
     */
    struct PatternDataNothing : public PatternData
    {
    private:
        friend class scam::MemoryManager;

        static PatternDataNothing * makeInstance();

    public:
        bool match(ScamValue arg, SyntaxMatchData & data) override;

        std::string identify() const override;
    };

    /*
     * Class for pattern identifiers.
     */
    struct PatternDataIdentifier : public PatternData
    {
    private:
        friend class scam::MemoryManager;
        PatternDataIdentifier(ScamValue identifier, bool rest);

        static PatternDataIdentifier *
        makeInstance(ScamValue identifier, bool rest = false);

    public:
        bool match(ScamValue arg, SyntaxMatchData & data) override;
        void tagAsEllipsis() override;

        bool isRest() const;

        void setMultiple();
        void getPatternIds(PatIDSet & patternIds) override;

        std::string identify() const override;

    private:
        std::string identifier;
        bool        rest;
        bool        multiple;
    };

    /*
     * Class for pattern sequences.
     */
    struct PatternDataSequence : public PatternData
    {
    private:
        friend class scam::MemoryManager;
        PatternDataSequence(const std::vector<PatternData *> & patterns);

        static PatternDataSequence *
        makeInstance(const std::vector<PatternData *> & patterns);

    public:
        void mark() override;

        bool match(ScamValue arg, SyntaxMatchData & data) override;
        void tagAsEllipsis() override;
        void getPatternIds(PatIDSet & patternIds) override;

        std::string identify() const override;

    private:
        std::vector<PatternData *> patterns;
    };

    /*
     * Class for literal values in patterns.
     */
    struct PatternDataLiteral : public PatternData
    {
    private:
        friend class scam::MemoryManager;
        PatternDataLiteral(ScamValue value);
        static PatternDataLiteral * makeInstance(ScamValue value);

    public:
        void mark() override;

        bool match(ScamValue arg, SyntaxMatchData & data) override;

        std::string identify() const override;

    private:
        ScamValue value;
    };
}

#endif
