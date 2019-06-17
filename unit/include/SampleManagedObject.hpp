#if ! defined(SAMPLEMANAGEDOBJECT_HPP)
#define SAMPLEMANAGEDOBJECT_HPP

#include "util/ManagedObject.hpp"

namespace scam
{
    class MemoryManager;

    namespace test_impl
    {
        /*
         * SampleManagedObject
         *
         * a subclass of ManagedObject to exercise the basic
         * functionality
         */
        class SampleManagedObject : public ManagedObject
        {
        private:
            friend class scam::MemoryManager;

            SampleManagedObject();
            explicit SampleManagedObject(int value);
            explicit SampleManagedObject(SampleManagedObject * proxy);

            static SampleManagedObject * makeInstance();
            static SampleManagedObject * makeInstance(int value);

            static SampleManagedObject *
            makeInstance(SampleManagedObject * proxy);

        public:
            void mark() override;
            int getValue() const;

        private:
            int value;
            SampleManagedObject * proxy;
        };
    }
}

#endif
