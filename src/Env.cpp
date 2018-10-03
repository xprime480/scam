
#include "Env.hpp"

#include "ScamException.hpp"

#include <iostream>
#include <map>
#include <sstream>

using namespace scam;
using namespace std;

namespace scam
{
    struct EnvData
    {
        map<string, shared_ptr<ScamExpr>> table;
        shared_ptr<EnvData> parent;

        void put(string const & key, shared_ptr<ScamExpr> val)
        {
            auto const iter = table.find(key);
            if ( iter != table.end() ) {
                stringstream s;
                s << "Key: " << key << " already exists in current frame";
                throw ScamException(s.str());
            }
            table[key] = val;
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

        shared_ptr<ScamExpr> get(string const & key) const
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

        void assign(string const & key, shared_ptr<ScamExpr> val)
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
                table[key] = val;
            }
        }

        void dump(size_t max)
        {
            if ( 0 == max ) {
                return;
            }

            cout << "[" << max << "]: " << this
                 << "\t" << parent.get() << "\n";
            if ( parent ) {
                parent->dump(max - 1 );
            }
        }
    };
}

string checkKey(shared_ptr<ScamExpr> key)
{
    if ( ! key->isSymbol() ) {
        stringstream s;
        s << "Environment key : " << key->toString() << " must be a symbol";
        throw ScamException(s.str());
    }
    return  key->toString();
}

Env::Env()
    : data(make_shared<EnvData>())
{
}

void Env::put(shared_ptr<ScamExpr> key, shared_ptr<ScamExpr> val)
{
    data->put(checkKey(key), val);
}

bool Env::check(std::shared_ptr<ScamExpr> key) const
{
    return data->check(checkKey(key));
}

shared_ptr<ScamExpr> Env::get(shared_ptr<ScamExpr> key) const
{
    return data->get(checkKey(key));
}

Env Env::extend()
{
    Env temp;
    temp.data->parent = this->data;
    return temp;
}

void Env::assign(shared_ptr<ScamExpr> key, shared_ptr<ScamExpr> val)
{
    data->assign(checkKey(key), val);
}

void Env::dump(size_t max) const
{
    data->dump(max);
}
