#if ! defined(CACHE_HPP)
#define CACHE_HPP 1

#include "ScamFwd.hpp"
#include "value/ScamData.hpp"

#include <map>

namespace scam
{
    template <typename KeyType>
    class Cache
    {
    public:
        ScamValue put(const KeyType & key, ScamValue value)
        {
            ScamValue old = get(key);
            if ( old ) {
                return old;
            }

            cache[key] = value;
            return value;
        }

        ScamValue get(const KeyType & key) const
        {
            const auto iter = cache.find(key);
            if ( cache.end() == iter ) {
                return nullptr;
            }
            return iter->second;
        }

        void mark() const
        {
            for ( const auto c : cache ) {
                c.second->mark();
            }
        }

        void release()
        {
            cache.clear();
        }

    private:
        std::map<KeyType, ScamValue> cache;
    };
}

#endif
