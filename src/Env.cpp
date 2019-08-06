#include "Env.hpp"

#include "ErrorCategory.hpp"
#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "util/MemoryManager.hpp"

#include <iostream>
#include <map>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    ScamValue checkKey(ScamValue key)
    {
        if ( nullptr == key || ! isSymbol(key) ) {
            if ( nullptr == key ) {
                key = makeString("<null pointer>");
            }
            ScamValue err =
                makeError("Non-symbol environment key not allowed %{0}", key);
            err->errorCategory() = envCategory;
            return err;
        }

        return makeNothing();
    }
}

Env::Env()
    : parent(nullptr)
{
}

Env * Env::makeInstance()
{
    return new Env();
}

void Env::mark()
{
    if ( ! isMarked() ) {
        ManagedObject::mark();
        if ( parent ) {
            parent->mark();
        }
        for ( const auto & p : table ) {
            p.second->mark();
        }
    }
}

ScamValue Env::put(ScamValue key, ScamValue val)
{
    ScamValue test = checkKey(key);
    if ( isError(test) ) {
        return test;
    }

    const string keyStr = key->stringValue();
    auto const iter = table.find(keyStr);
    if ( iter != table.end() ) {
        ScamValue err =
            makeError("Key: '%{0}' already exists in current frame", key);
        err->errorCategory() = envCategory;
        return err;
    }

    table[keyStr] = val;

    return makeNothing();
}

ScamValue Env::check(ScamValue key, bool checkParent) const
{
    ScamValue test = checkKey(key);
    if ( isError(test) ) {
        return test;
    }

    const string keyStr = key->stringValue();
    auto const iter = table.find(keyStr);
    if ( iter != table.end() ) {
        return makeBoolean(true);
    }
    if ( checkParent && parent ) {
        return parent->check(key, checkParent);
    }

    return makeBoolean(false);
}

ScamValue Env::get(ScamValue key) const
{
    ScamValue test = checkKey(key);
    if ( isError(test) ) {
        return test;
    }

    const string keyStr = key->stringValue();
    auto const iter = table.find(keyStr);
    if ( iter != table.end() ) {
        return iter->second;
    }
    if ( parent ) {
        return parent->get(key);
    }

    ScamValue err = makeError("Key: %{0} does not exist for reading", key);
    err->errorCategory() = envCategory;
    return err;
}

void Env::reset()
{
    map<string, ScamValue> temp;
    table.swap(temp);

    if ( parent ) {
        parent->reset();
    }
}

Env * Env::extend() const
{
    Env * temp = standardMemoryManager.make<Env>();
    temp->parent = const_cast<Env *>(this);
    return temp;
}

Env * Env::getParent() const
{
    return parent;
}

Env * Env::getTop() const
{
    if ( ! parent ) {
        return const_cast<Env *>(this);
    }

    return parent->getTop();
}

ScamValue Env::assign(ScamValue key, ScamValue val)
{
    ScamValue test = checkKey(key);
    if ( isError(test) ) {
        return test;
    }

    const string keyStr = key->stringValue();
    auto const iter = table.find(keyStr);
    if ( iter == table.end() ) {
        if ( parent ) {
            return parent->assign(key, val);
        }
        else {
            ScamValue err =
                makeError("Key: %{0} does not exist for assignment", key);
            err->errorCategory() = envCategory;
            return err;
        }
    }
    else {
        table[keyStr] = val;
    }

    return makeNothing();
}

ScamValue Env::remove(ScamValue key)
{
    ScamValue test = checkKey(key);
    if ( isError(test) ) {
        return test;
    }

    const string keyStr = key->stringValue();
    auto const iter = table.find(keyStr);
    if ( iter != table.end() ) {
        table.erase(iter);
    }

    return makeNothing();
}

void Env::dump(size_t max, bool full) const
{
    if ( 0 == max ) {
        return;
    }

    cerr << "[" << max << "]: " << this
         << "\t" << parent << "\n";

    if ( full ) {
        for ( const auto kv : table ) {
            cerr << "\t" << kv.first << "\t" << writeValue(kv.second) << "\n";
        }
    }

    if ( parent ) {
        parent->dump(max - 1, full);
    }
}
