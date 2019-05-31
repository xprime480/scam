#include "TestBase.hpp"

#include "input/CharStream.hpp"
#include "input/PortCharStream.hpp"
#include "input/StringCharStream.hpp"
#include "port/FilePort.hpp"
#include "port/FixedStringPort.hpp"
#include "port/StringPort.hpp"

using namespace std;
using namespace scam;

class CharStreamTest : public TestBase
{
protected:
    CharStreamTest()
        : TestBase(false)
    {
    }

    static constexpr unsigned int RO = ScamPort::Readable;
    static constexpr unsigned int WO = ScamPort::Writeable;
    static constexpr unsigned int RW = RO | WO;
};

TEST_F(CharStreamTest, StringCharStreamEmpty)
{
    string text { "" };
    StringCharStream stream(text);

    EXPECT_EQ(0, stream.peek());
    EXPECT_EQ(text, stream.strPeek(9999));
    PositionType start = stream.getPos();
    EXPECT_EQ(0, stream.getCurrent());
    stream.advance(9999);
    PositionType end = stream.getPos();
    EXPECT_EQ(start, end);
}

TEST_F(CharStreamTest, StringCharStreamFull)
{
    string text { "the rain in spain" };
    StringCharStream stream(text);

    EXPECT_EQ('t', stream.peek());
    EXPECT_EQ(string("the"), stream.strPeek(3));
    EXPECT_EQ('t', stream.getCurrent());

    PositionType start = stream.getPos();
    stream.advance(5);
    PositionType end = stream.getPos();
    stream.advance(5);

    EXPECT_EQ(string("he rain in spain"), stream.allInput(start));
    EXPECT_EQ(string("he rain in"), stream.strBetween(start));
    EXPECT_EQ(string("he ra"), stream.strBetween(start, end));

    stream.setPos(start);
    EXPECT_EQ(string("he rain"), stream.strPeek(7));
}

TEST_F(CharStreamTest, StringPortCharStreamEmpty)
{
    string text { "" };
    ScamValue expr = makePort(new FixedStringPort(text.c_str()));
    PortCharStream stream(expr);

    EXPECT_EQ(0, stream.peek());
    EXPECT_EQ(text, stream.strPeek(9999));
    PositionType start = stream.getPos();
    EXPECT_EQ(0, stream.getCurrent());
    stream.advance(9999);
    PositionType end = stream.getPos();
    EXPECT_EQ(start, end);
}

TEST_F(CharStreamTest, StringPortCharStreamFull)
{
    string text { "the rain in spain" };
    ScamValue expr = makePort(new FixedStringPort(text.c_str()));
    PortCharStream stream(expr);

    EXPECT_EQ('t', stream.peek());
    EXPECT_EQ(string("the"), stream.strPeek(3));
    EXPECT_EQ('t', stream.getCurrent());

    PositionType start = stream.getPos();
    stream.advance(5);
    PositionType end = stream.getPos();
    stream.advance(5);

    EXPECT_EQ(string("he rain in spain"), stream.allInput(start));
    EXPECT_EQ(string("he rain in"), stream.strBetween(start));
    EXPECT_EQ(string("he ra"), stream.strBetween(start, end));

    stream.setPos(start);
    EXPECT_EQ(string("he rain"), stream.strPeek(7));
}

TEST_F(CharStreamTest, FilePortCharStreamEmpty)
{
    string text { "" };
    const char * path = "scripts/empty.txt";
    ScamValue expr = makePort(new FilePort(path, RO));
    PortCharStream stream(expr);

    EXPECT_EQ(0, stream.peek());
    EXPECT_EQ(text, stream.strPeek(9999));
    PositionType start = stream.getPos();
    EXPECT_EQ(0, stream.getCurrent());
    stream.advance(9999);
    PositionType end = stream.getPos();
    EXPECT_EQ(start, end);
}

TEST_F(CharStreamTest, FilePortCharStreamFull)
{
    const char * path = "scripts/rain.txt";
    ScamValue expr = makePort(new FilePort(path, RO));
    PortCharStream stream(expr);

    EXPECT_EQ('t', stream.peek());
    EXPECT_EQ(string("the"), stream.strPeek(3));
    EXPECT_EQ('t', stream.getCurrent());

    PositionType start = stream.getPos();
    stream.advance(5);
    PositionType end = stream.getPos();
    stream.advance(5);

    EXPECT_EQ(string("he rain in spain\n\n"), stream.allInput(start));
    EXPECT_EQ(string("he rain in"), stream.strBetween(start));
    EXPECT_EQ(string("he ra"), stream.strBetween(start, end));

    stream.setPos(start);
    EXPECT_EQ(string("he rain"), stream.strPeek(7));
}

TEST_F(CharStreamTest, StringPortCharStreamChanging)
{
    StringPort * port = new StringPort("", RW);
    ScamValue expr = makePort(port);
    PortCharStream stream(expr);

    EXPECT_EQ(0, stream.peek());
    port->putChar('x');
    EXPECT_EQ("x", stream.strPeek(3));

    PositionType start = stream.getPos();
    stream.advance(5);
    port->put("abcdefghijklm", 13);
    stream.advance(5);
    PositionType end = stream.getPos();
    stream.advance(5);

    EXPECT_EQ(string("xabcdefghijklm"), stream.allInput(start));
    EXPECT_EQ(string("xabcdefghij"), stream.strBetween(start));
    EXPECT_EQ(string("xabcde"), stream.strBetween(start, end));

    stream.setPos(start);
    EXPECT_EQ(string("xabcdef"), stream.strPeek(7));
}
