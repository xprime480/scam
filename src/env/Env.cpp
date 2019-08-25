#include "env/Env.hpp"

#include "ErrorCategory.hpp"
#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "util/DebugTrace.hpp"
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

Env::~Env()
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

ScamValue Env::merge(Env * other)
{
    if ( other ) {
        for ( const auto kv : other->table ) {
            ScamValue key = makeSymbol(kv.first);
            ScamValue val = kv.second;
            ScamValue test = check(key, false);
            if ( truth(test) ) {
                (void) assign(key, val);
            }
            else {
                (void) put(key, val);
            }
        }
    }

    return makeNothing();
}

void Env::getKeys(set<string> & keys)
{
    for ( const auto kv : table ) {
        keys.insert(kv.first);
    }
}

void Env::dump(size_t max, bool full, bool defs) const
{
    if ( 0 == max ) {
        return;
    }

    stringstream s;
    s << "[" << max << "]: " << this
      << "\t" << parent << "\n";

    if ( full ) {
        for ( const auto kv : table ) {
            s << "\t" << kv.first;
            if ( defs ) {
                s << "\t" << writeValue(kv.second);
            }
            s << "\n";
        }
    }

    scamLog(s.str());

    if ( parent ) {
        parent->dump(max - 1, full, defs);
    }
}
