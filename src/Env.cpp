
#include "Env.hpp"

#include "ScamException.hpp"
#include "expr/ScamExpr.hpp"

#include <iostream>
#include <map>
#include <sstream>

using namespace scam;
using namespace std;

namespace scam
{
    struct EnvData
    {
        map<string, ExprHandle> table;
        shared_ptr<EnvData> parent;

        void reset()
        {
            map<string, ExprHandle> temp;
            table.swap(temp);
            if ( parent.get() ) {
                parent->reset();
            }
        }

        void put(string const & key, ScamExpr * val)
        {
            auto const iter = table.find(key);
            if ( iter != table.end() ) {
                stringstream s;
                s << "Key: " << key << " already exists in current frame";
                throw ScamException(s.str());
            }
            table[key] = val->clone();
        }

        bool check(string const & key) const
        {
            auto const iter = table.find(key);
            if ( iter != table.end() ) {
                return true;
            }
            if ( parent ) {
                return parent->check(key);
            }

            return false;
        }

        ExprHandle get(string const & key) const
        {
            auto const iter = table.find(key);
            if ( iter != table.end() ) {
                return iter->second;
            }
            if ( parent ) {
                return parent->get(key);
            }

            stringstream s;
            s << "Key: " << key << " does not exist for reading";
            throw ScamException(s.str());
        }

        void assign(string const & key, ScamExpr * val)
        {
            auto const iter = table.find(key);
            if ( iter == table.end() ) {
                if ( parent ) {
                    parent->assign(key, val);
                }
                else {
                    stringstream s;
                    s << "Key: " << key << " does not exist for assignment";
                    throw ScamException(s.str());
                }
            }
            else {
                table[key] = val->clone();
            }
        }

        void dump(size_t max, bool full)
        {
            if ( 0 == max ) {
                return;
            }

            cerr << "[" << max << "]: " << this
                 << "\t" << parent.get() << "\n";

            if ( full ) {
                for ( const auto kv : table ) {
                    cerr << "\t" << kv.first << "\t"
                         << kv.second->toString() << "\n";
                }
            }

            if ( parent ) {
                parent->dump(max - 1, full);
            }
        }
    };
}

namespace
{
    string checkKey(ScamExpr const * key)
    {
        if ( ! key->isSymbol() ) {
            stringstream s;
            s << "Environment key : " << key->toString() << " must be a symbol";
            throw ScamException(s.str());
        }
        return  key->toString();
    }
}

Env::Env()
    : data(make_shared<EnvData>())
{
}

void Env::put(ScamExpr const * key, ScamExpr * val)
{
    data->put(checkKey(key), val);
}

bool Env::check(ScamExpr const * key) const
{
    return data->check(checkKey(key));
}

ExprHandle Env::get(ScamExpr const * key) const
{
    return data->get(checkKey(key));
}

void Env::reset()
{
    data->reset();
}

Env Env::extend() const
{
    Env temp;
    temp.data->parent = this->data;
    return temp;
}

Env Env::parent() const
{
    Env temp;
    temp.data = this->data->parent;
    return temp;
}

Env Env::top() const
{
    if ( ! this->data->parent ) {
        return *this;
    }
    return parent().top();
}

void Env::assign(ScamExpr const * key, ScamExpr * val)
{
    data->assign(checkKey(key), val);
}

void Env::dump(size_t max, bool full) const
{
    data->dump(max, full);
}
