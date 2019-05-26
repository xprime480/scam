#include "Env.hpp"

#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "util/MemoryManager.hpp"

#include <iostream>
#include <map>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    string checkKey(ScamValue key)
    {
        if ( nullptr == key || ! isSymbol(key) ) {
            stringstream s;
            s << "Null Environment key not allowed";
            throw ScamException(s.str());
        }
        return  writeValue(key);
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

void Env::mark() const
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

void Env::put(ScamValue key, ScamValue val)
{
    const string keyStr = checkKey(key);
    auto const iter = table.find(keyStr);
    if ( iter != table.end() ) {
        stringstream s;
        s << "Key: '" << keyStr << "' already exists in current frame";
        throw ScamException(s.str());
    }

    table[keyStr] = val;
}

bool Env::check(ScamValue key, bool checkParent) const
{
    const string keyStr = checkKey(key);
    auto const iter = table.find(keyStr);
    if ( iter != table.end() ) {
        return true;
    }
    if ( checkParent && parent ) {
        return parent->check(key, checkParent);
    }

    return false;
}

ScamValue Env::get(ScamValue key) const
{
    const string keyStr = checkKey(key);
    auto const iter = table.find(keyStr);
    if ( iter != table.end() ) {
        return iter->second;
    }
    if ( parent ) {
        return parent->get(key);
    }

    stringstream s;
    s << "Key: " << keyStr << " does not exist for reading";
    throw ScamException(s.str());
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

void Env::assign(ScamValue key, ScamValue val)
{
    const string keyStr = checkKey(key);
    auto const iter = table.find(keyStr);
    if ( iter == table.end() ) {
        if ( parent ) {
            parent->assign(key, val);
        }
        else {
            stringstream s;
            s << "Key: " << keyStr << " does not exist for assignment";
            throw ScamException(s.str());
        }
    }
    else {
        table[keyStr] = val;
    }
}

void Env::remove(ScamValue key)
{
    const string keyStr = checkKey(key);
    auto const iter = table.find(keyStr);
    if ( iter != table.end() ) {
        table.erase(iter);
    }
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
