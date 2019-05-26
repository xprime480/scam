#include "TestLoader.hpp"

#include <fstream>
#include <iostream>

using namespace std;
using namespace scam;
using namespace scam::test;

TestLoader::TestLoader(char const * script)
    : mode(NONE)
    , skip(false)
    , ok(false)
    , linesToKeep(0u)
{
    //    std::cerr << "Script: " << script << "\n";

    testspec.open(script);
    std::string line;

    while ( getNextLine(line) ) {
        if ( setMode(line) ) {
            if ( Mode::SKIP == mode ) {
                skip = true;
                return;
            }
        }
        else {
            handleLine(line);
        }
    }

    ok = Mode::RESULT == mode;
}

bool TestLoader::isSkipSet() const
{
    return skip;
}

bool TestLoader::isOK() const
{
    return skip || ok;
}

void TestLoader::getComponents(std::string & name,
                               std::string & input,
                               std::string & expected) const
{
    name = this->name.str();
    input = this->input.str();
    expected = this->expected.str();
}

size_t TestLoader::getLinesToKeep() const
{
    return linesToKeep;
}

bool TestLoader::getNextLine(std::string & line)
{
    char buf[1024];

    if ( testspec.good() && ! testspec.eof() ) {
        testspec.getline(buf, sizeof ( buf ));
        line = std::string(buf);
        return true;
    }

    return false;
}

bool TestLoader::setMode(std::string const & line)
{
    if ( line == "[skip]" ) {
        skip = true;
        mode = Mode::SKIP;
    }
    else if ( line == "[name]" ) {
        mode = Mode::NAME;
    }
    else if ( line == "[input]" ) {
        mode = Mode::INPUT;
    }
    else if ( line == "[result]" ) {
        mode = Mode::RESULT;
    }
    else if ( line.substr(0, 6) == "[keep " ) {
        linesToKeep = atoi(line.substr(6, line.size() - 7).c_str());
    }
    else {
        return false;
    }

    return true;
}

void TestLoader::handleLine(std::string const & line)
{
    switch ( mode ) {
    case Mode::NAME:
        name << line;
        mode = Mode::NONE;
        break;

    case Mode::INPUT:
        input << line << "\n";
        break;

    case Mode::RESULT:
        expected << line << "\n";
        break;

    default:
        break;
    }
}
