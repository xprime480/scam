#include "TestBase.hpp"

#include "ScamException.hpp"
#include "expr/ValueFactory.hpp"
#include "port/FilePort.hpp"
#include "port/FixedStringPort.hpp"
#include "port/StringPort.hpp"

#include <memory>

using namespace std;
using namespace scam;

class PortTest : public TestBase
{
protected:
    PortTest()
    {
    }

    static constexpr unsigned int RO = ScamPort::Readable;
    static constexpr unsigned int WO = ScamPort::Writeable;
    static constexpr unsigned int RW = RO | WO;
};

TEST_F(PortTest, ReadFileNotFound)
{
    EXPECT_THROW(new FilePort("I-hope-this-is-not-a-file", RO), ScamException);
}

TEST_F(PortTest, ReadFileEmpty)
{
    const char * path = "scripts/empty.txt";
    unique_ptr<ScamPort> port { new FilePort(path, RO) };

    ASSERT_NE(nullptr, port.get());

    ScamValue expr = port->getContents();
    expectString(expr, "\"?\"");

    EXPECT_FALSE(port->eof());
    EXPECT_EQ('\0', port->getChar());
    EXPECT_TRUE(port->eof());
}

TEST_F(PortTest, ReadFileNonEmpty)
{
    const char * path = "scripts/nonempty.txt";
    unique_ptr<ScamPort> port { new FilePort(path, RO) };

    ASSERT_NE(nullptr, port.get());

    ScamValue expr = port->getContents();
    expectString(expr, "\"?\"");

    EXPECT_FALSE(port->eof());
    EXPECT_EQ('a', port->getChar());

    char buffer[10];
    size_t count = port->get(buffer, 10);
    EXPECT_EQ(3, count);
    EXPECT_EQ('\n', buffer[2]);

    EXPECT_TRUE(port->eof());
}

TEST_F(PortTest, WriteFile)
{
    const char * path = "/tmp/test.txt";
    unique_ptr<ScamPort> port { new FilePort(path, WO) };

    ASSERT_NE(nullptr, port.get());

    ScamValue expr = port->getContents();
    expectString(expr, "\"?\"");

    EXPECT_FALSE(port->eof());
    EXPECT_THROW(port->getChar(), ScamException);

    EXPECT_EQ(7, port->put("abcdef\n", 7));
}

TEST_F(PortTest, FilePortAsValue)
{
    ScamValue expr = makePort(new FilePort("/tmp/test2.txt", WO));
    expectPort(expr, "file-port</tmp/test2.txt>", "\"?\"");
}

TEST_F(PortTest, ReadWriteStringPortInitiallyEmpty)
{
    unique_ptr<ScamPort> port { new StringPort("", RW) };

    ASSERT_NE(nullptr, port.get());

    ScamValue expr = port->getContents();
    expectString(expr, "\"\"");

    EXPECT_FALSE(port->eof());
    EXPECT_EQ('\0', port->getChar());

    port->putChar('x');
    EXPECT_EQ('x', port->getChar());
}

TEST_F(PortTest, ReadWriteStringPortWithInitialData)
{
    unique_ptr<ScamPort> port { new StringPort("abc", RW) };

    ASSERT_NE(nullptr, port.get());

    ScamValue expr = port->getContents();
    expectString(expr, "\"abc\"");

    EXPECT_EQ('a', port->getChar());
    EXPECT_EQ('b', port->getChar());
    EXPECT_EQ('c', port->getChar());

    EXPECT_FALSE(port->eof());
    EXPECT_EQ('\0', port->getChar());

    expr = port->getContents();
    expectString(expr, "\"\"");

    port->putChar('x');
    EXPECT_EQ('x', port->getChar());
    EXPECT_EQ('\0', port->getChar());
}

TEST_F(PortTest, ReadOnlyStringPort)
{
    unique_ptr<ScamPort> port { new StringPort("abc", RO) };

    ASSERT_NE(nullptr, port.get());

    ScamValue expr = port->getContents();
    expectString(expr, "\"abc\"");

    EXPECT_EQ('a', port->getChar());
    EXPECT_EQ('b', port->getChar());
    EXPECT_EQ('c', port->getChar());

    EXPECT_TRUE(port->eof());
    EXPECT_EQ('\0', port->getChar());

    EXPECT_THROW(port->putChar('x'), ScamException);

    expr = port->getContents();
    expectString(expr, "\"\"");
}

TEST_F(PortTest, WriteOnlyStringPort)
{
    unique_ptr<ScamPort> port { new StringPort("abc", WO) };

    ASSERT_NE(nullptr, port.get());

    EXPECT_TRUE(port->eof());

    ScamValue expr = port->getContents();
    expectString(expr, "\"abc\"");

    EXPECT_THROW(port->getChar(), ScamException);
    port->putChar('x');

    expr = port->getContents();
    expectString(expr, "\"abcx\"");
}

TEST_F(PortTest, ReadWriteStringPortMultipleChars)
{
    unique_ptr<ScamPort> port { new StringPort("", RW) };

    ASSERT_NE(nullptr, port.get());

    ScamValue expr = port->getContents();
    expectString(expr, "\"\"");

    EXPECT_EQ(6, port->put("abcdefghij", 6));

    expr = port->getContents();
    expectString(expr, "\"abcdef\"");

    EXPECT_EQ('a', port->getChar());

    char buffer[10];
    size_t count = port->get(buffer, 10);
    EXPECT_EQ(5, count);
    EXPECT_EQ('f', buffer[4]);

    expr = port->getContents();
    expectString(expr, "\"\"");

    EXPECT_FALSE(port->eof());
}

TEST_F(PortTest, StringPortAsValue)
{
    ScamValue expr = makePort(new StringPort("cat", RW));
    expectPort(expr, "string-port<cat>", "\"cat\"");
}

TEST_F(PortTest, FixedStringPortInitiallyEmpty)
{
    unique_ptr<ScamPort> port { new FixedStringPort("") };

    ASSERT_NE(nullptr, port.get());

    ScamValue expr = port->getContents();
    expectString(expr, "\"\"");

    EXPECT_TRUE(port->eof());
    EXPECT_EQ('\0', port->getChar());

    EXPECT_THROW(port->putChar('x'), ScamException);
}

TEST_F(PortTest, FixedStringPortWithInitialData)
{
    unique_ptr<ScamPort> port { new FixedStringPort("abc") };

    ASSERT_NE(nullptr, port.get());

    ScamValue expr = port->getContents();
    expectString(expr, "\"abc\"");

    EXPECT_EQ('a', port->getChar());
    EXPECT_EQ('b', port->getChar());

    EXPECT_FALSE(port->eof());
    EXPECT_EQ('c', port->getChar());
    EXPECT_EQ('\0', port->getChar());
    EXPECT_TRUE(port->eof());

    expr = port->getContents();
    expectString(expr, "\"\"");
}

TEST_F(PortTest, FixedStringPortMultipleChars)
{
    unique_ptr<ScamPort> port { new FixedStringPort("abcdef") };

    ASSERT_NE(nullptr, port.get());

    ScamValue expr = port->getContents();
    expectString(expr, "\"abcdef\"");

    EXPECT_EQ('a', port->getChar());

    char buffer[10];
    size_t count = port->get(buffer, 10);
    EXPECT_EQ(5, count);
    EXPECT_EQ('f', buffer[4]);

    expr = port->getContents();
    expectString(expr, "\"\"");
}

TEST_F(PortTest, FixedStringPortAsValue)
{
    ScamValue expr = makePort(new FixedStringPort("cat"));
    expectPort(expr, "fixed-string-port<cat>", "\"cat\"");
}

