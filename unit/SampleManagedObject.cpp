#include "SampleManagedObject.hpp"

using namespace scam::test_impl;

SampleManagedObject::SampleManagedObject()
    : value(7)
    , proxy(nullptr)
{
}

SampleManagedObject::SampleManagedObject(int value)
    : value(value)
    , proxy(nullptr)
{
}

SampleManagedObject::SampleManagedObject(SampleManagedObject * proxy)
    : value(0)
    , proxy(proxy)
{
}

SampleManagedObject * SampleManagedObject::makeInstance()
{
    return new SampleManagedObject();
}

SampleManagedObject * SampleManagedObject::makeInstance(int value)
{
    return new SampleManagedObject(value);
}

SampleManagedObject *
SampleManagedObject::makeInstance(SampleManagedObject * proxy)
{
    return new SampleManagedObject(proxy);
}

void SampleManagedObject::mark() const
{
    if ( ! isMarked() ) {
        ManagedObject::mark();
        if ( proxy ) {
            proxy->mark();
        }
    }
}

int SampleManagedObject::getValue() const
{
    if ( proxy ) {
        return proxy->getValue();
    }
    return value;
}
