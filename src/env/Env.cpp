#include "env/Env.hpp"

#include "ErrorCategory.hpp"
#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamToInternal.hpp"
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

    ScamValue rv = makeNothing();

    const string keyStr = key->stringValue();
    auto const iter = table.find(keyStr);

    if ( iter == table.end() ) {
        table[keyStr] = val;
    }
    else {
        rv = makeError("Key: '%{0}' already exists in current frame", key);
        rv->errorCategory() = envCategory;
    }

    return rv;
}

ScamValue Env::check(ScamValue key, bool checkParent) const
{
    ScamValue test = checkKey(key);
    if ( isError(test) ) {
        return test;
    }

    ScamValue rv = makeBoolean(false);

    const string keyStr = key->stringValue();
    auto const iter = table.find(keyStr);

    if ( iter == table.end() ) {
        if ( checkParent && parent ) {
            rv = parent->check(key, checkParent);
        }
    }
    else {
        ScamValue val = iter->second;
        if ( isForwarder(val) ) {
            rv = asEnv(val)->check(key, checkParent);
        }
        else {
            rv = makeBoolean(true);
        }
    }

    return rv;
}

ScamValue Env::get(ScamValue key) const
{
    ScamValue test = checkKey(key);
    if ( isError(test) ) {
        return test;
    }

    ScamValue rv = makeNothing();

    const string keyStr = key->stringValue();
    auto const iter = table.find(keyStr);

    if ( iter == table.end() ) {
        if ( parent ) {
            return parent->get(key);
        }
        else {
            rv = makeError("Key: %{0} does not exist for reading", key);
            rv->errorCategory() = envCategory;
        }
    }
    else {
        rv = iter->second;
        if ( isForwarder(rv) ) {
            rv = asEnv(rv)->get(key);
        }
    }

    return rv;
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

    ScamValue rv = makeNothing();

    const string keyStr = key->stringValue();
    auto const iter = table.find(keyStr);

    if ( iter == table.end() ) {
        if ( parent ) {
            rv = parent->assign(key, val);
        }
        else {
            rv = makeError("Key: %{0} does not exist for assignment", key);
            rv->errorCategory() = envCategory;
        }
    }
    else {
        ScamValue old = table[keyStr];
        if ( isForwarder(old) ) {
            rv = asEnv(old)->assign(key, val);
        }
        else {
            table[keyStr] = val;
        }
    }

    return rv;
}

ScamValue Env::remove(ScamValue key)
{
    ScamValue test = checkKey(key);
    if ( isError(test) ) {
        return test;
    }

    ScamValue rv = makeNothing();

    const string keyStr = key->stringValue();
    auto const iter = table.find(keyStr);

    if ( iter != table.end() ) {
        ScamValue val = iter->second;
        if ( isForwarder(val) ) {
            rv = asEnv(val)->remove(key);
        }
        table.erase(iter);
    }

    return rv;
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
