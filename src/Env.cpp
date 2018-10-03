
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
        map<shared_ptr<ScamExpr>, shared_ptr<ScamExpr>> table;
        shared_ptr<EnvData> parent;

        void put(shared_ptr<ScamExpr> key, shared_ptr<ScamExpr> val)
        {
            auto const iter = table.find(key);
            if ( iter != table.end() ) {
                stringstream s;
                s << "Key: " << key->toString()
                  << " already exists in current frame";
                throw ScamException(s.str());
            }
            table[key] = val;
        }

        shared_ptr<ScamExpr> get(shared_ptr<ScamExpr> key) const
        {
            auto const iter = table.find(key);
            if ( iter != table.end() ) {
                return iter->second;
            }
            if ( parent ) {
                return parent->get(key);
            }

            stringstream s;
            s << "Key: " << key->toString() << " does not exist for reading";
            throw ScamException(s.str());
        }

        void assign(shared_ptr<ScamExpr> key, shared_ptr<ScamExpr> val)
        {
            auto const iter = table.find(key);
            if ( iter == table.end() ) {
                if ( parent ) {
                    parent->assign(key, val);
                }
                else {
                    stringstream s;
                    s << "Key: " << key->toString()
                      << " does not exist for assignment";
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

Env::Env()
    : data(make_shared<EnvData>())
{
}

void Env::put(shared_ptr<ScamExpr> key, shared_ptr<ScamExpr> val)
{
    data->put(key, val);
}

shared_ptr<ScamExpr> Env::get(shared_ptr<ScamExpr> key) const
{
    return data->get(key);
}

Env Env::extend()
{
    Env temp;
    temp.data->parent = this->data;
    return temp;
}

void Env::assign(shared_ptr<ScamExpr> key, shared_ptr<ScamExpr> val)
{
    data->assign(key, val);
}

void Env::dump(size_t max) const
{
    data->dump(max);
}
