#include "prim/Primitive.hpp"

#include "WorkQueue.hpp"
#include "prim/PrimWorker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Primitive::Primitive(string const & name)
    : ScamData(ScamData::Primitive)
{
    STRVAL(this) = name;
}
