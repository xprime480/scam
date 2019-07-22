#if ! defined(TEMPLATEDATA_HPP)
#define TEMPLATEDATA_HPP 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"
#include "form/SyntaxMatchData.hpp"

#include <string>
#include <vector>

namespace scam
{
    /*
     * Base class for describing parts of templates
     */
    class TemplateData : public ManagedObject
    {
    public:
        virtual ~TemplateData();

        virtual ScamValue expand(const SyntaxMatchData & data) = 0;
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

    private:
        std::vector<TemplateData *> templates;
    };
}

#endif
