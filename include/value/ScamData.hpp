#if ! defined(SCAMDATA_HPP)
#define SCAMDATA_HPP 1

#include "util/ManagedObject.hpp"

#include "ScamException.hpp"
#include "ScamFwd.hpp"
#include "util/ClassDef.hpp"
#include "util/LambdaDef.hpp"
#include "value/ScamValueType.hpp"
#include "value/ValueWriter.hpp"

#include <functional>
#include <sstream>
#include <string>
#include <vector>

namespace scam
{
    class ScamPort;
    class SyntaxRules;

    using SfFunction   = std::function<void(ScamValue, Continuation *, Env *)>;
    using PrimFunction = std::function<void(ScamValue, Continuation *)>;

    namespace ScamData__impl
    {
        struct ScamContents;
    }

    struct ScamData : public ManagedObject
    {
    public:
        explicit ScamData(ScamValueType type, bool managed = true);
        ~ScamData();

        ScamData(const ScamData &) = delete;
        ScamData operator=(const ScamData &) = delete;
        ScamData(ScamData &&) = delete;
        ScamData operator=(ScamData &&) = delete;

        static ScamData * makeInstance(ScamValueType type, bool managed = true);

        void mark() override final;

        void makeImmutable();
        bool isImmutable() const;

        ScamValue setMeta(std::string const & key, ScamValue value) const;
        ScamValue hasMeta(std::string const & key) const;
        ScamValue getMeta(std::string const & key) const;

        /**
         * member data
         */
        const ScamValueType type;
        mutable Env * metadata;

        bool & boolValue();
        unsigned char & charValue();
        std::string & stringValue();

        std::string & errorMessage();
        std::vector<ScamValue> & errorIrritants();
        bool & errorHandled();
        ScamValue & errorCategory();

        ScamValue & carValue();
        ScamValue & cdrValue();

        std::vector<ScamValue> & vectorData();
        std::vector<unsigned char> & byteVectorData();

        std::vector<ScamValue> & multipleValues();

        Continuation *& contValue();
        ScamPort *& portValue();
        Env *& envValue();
        SyntaxRules & syntaxRules();

        std::vector<ScamValue> & dictKeys();
        std::vector<ScamValue> & dictValues();

        LambdaDef & closureDef();
        Env *& closureEnv();

        ClassDef & classDef();
        Env *& classEnv();

        Env *& instancePrivate();
        Env *& instanceLocal();

        std::string & primName();
        PrimFunction & primFunc();

        std::string & sfName();
        SfFunction & sfFunc();

        ScamValue & placeholderValue();

        /**********/

        bool & exactFlag();
        ScamValue & realPart();
        ScamValue & imagPart();
        double & realValue();
        int & numPart();
        int & denPart();
        int & intPart();

    private:
        bool immutable;
        ScamData__impl::ScamContents * contents;

        template<typename T>
        T* assertContentsType()
        {
            T* temp = dynamic_cast<T*>(contents);

            if ( ! temp ) {
                std::stringstream s;
                s << "InternalError, type assertion in ScamData: expected: "
                  << T::name
                  << "; operating on "
                  << describe(type);

                throw ScamException(s.str());
            }

            return temp;
        }

    };
}

#endif
