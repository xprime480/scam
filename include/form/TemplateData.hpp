#if ! defined(TEMPLATEDATA_HPP)
#define TEMPLATEDATA_HPP 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"
#include "form/SyntaxMatchData.hpp"

#include <set>
#include <string>
#include <vector>

namespace scam
{
    using IDSet = std::set<std::string>;

    /*
     * Base class describing parts of templates
     */
    class TemplateData : public ManagedObject
    {
    public:
        virtual ~TemplateData();

        virtual ScamValue expand(const SyntaxMatchData & data) = 0;
        virtual ScamValue expandCount(const SyntaxMatchData & data, int n) = 0;
        virtual void getPatternIds(IDSet & identifiers) const;
        virtual void getTemplateIds(IDSet & identifiers) const;

        virtual std::set<ScamValue> getFreeSymbols() const;

        virtual std::string identify() const = 0;
    };

    /*
     * Class for literal data in template.
     */
    class TemplateDataLiteral : public TemplateData
    {
    private:
        friend class scam::MemoryManager;
        TemplateDataLiteral(ScamValue value);
        static TemplateDataLiteral * makeInstance(ScamValue value);

    public:
        void mark() override;

        ScamValue expand(const SyntaxMatchData & data) override;
        ScamValue expandCount(const SyntaxMatchData & data, int n) override;

        void getTemplateIds(IDSet & identifiers) const override;

        std::string identify() const override;

    private:
        ScamValue value;
    };

    /*
     * Class for pattern variable to be substituted at expansion time.
     */
    class TemplateDataIdentifier : public TemplateData
    {
    private:
        friend class scam::MemoryManager;
        TemplateDataIdentifier(ScamValue value);
        static TemplateDataIdentifier * makeInstance(ScamValue value);

    public:
        ScamValue expand(const SyntaxMatchData & data) override;
        ScamValue expandCount(const SyntaxMatchData & data, int n) override;
        void getPatternIds(IDSet & identifiers) const override;

        std::string identify() const override;

    private:
        std::string identifier;
    };

    /*
     * Class for lists, each of whose components to be expanded.
     */
    class TemplateDataList : public TemplateData
    {
    private:
        friend class scam::MemoryManager;
        TemplateDataList(std::vector<TemplateData *> templates);

        static TemplateDataList *
        makeInstance(std::vector<TemplateData *> templates);

    public:
        void mark() override;

        ScamValue expand(const SyntaxMatchData & data) override;
        ScamValue expandCount(const SyntaxMatchData & data, int n) override;
        void getPatternIds(IDSet & identifiers) const override;
        void getTemplateIds(IDSet & identifiers) const override;

        std::string identify() const override;

    private:
        std::vector<TemplateData *> templates;
    };

    /*
     * Class for ellipsis pattern, which may be expanded 0+ times
     */
    class TemplateDataEllipsis : public TemplateData
    {
    private:
        friend class scam::MemoryManager;
        TemplateDataEllipsis(TemplateData * subTemplate);

        static TemplateDataEllipsis * makeInstance(TemplateData * subTemplate);

    public:
        void mark() override;

        ScamValue expand(const SyntaxMatchData & data) override;
        ScamValue expandCount(const SyntaxMatchData & data, int n) override;
        void getPatternIds(IDSet & identifiers) const override;
        void getTemplateIds(IDSet & identifiers) const override;

        std::string identify() const override;

    private:
        TemplateData * subTemplate;

        ScamValue noIdentifiers() const;
    };
}

#endif
