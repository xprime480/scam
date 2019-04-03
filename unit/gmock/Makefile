
GTEST_DIR= ../../../googletest/googletest
CPP = /usr/bin/g++
INC = -isystem $(GTEST_DIR)/include -I$(GTEST_DIR)
CPPFLAGS =  -g -Wall $(INC)

all : libgtest.a

libgtest.a : $(GTEST_DIR)/src/gtest-all.cc $(GTEST_DIR)/src/gtest_main.cc
	$(CPP) $(CPPFLAGS) -pthread -c $(GTEST_DIR)/src/gtest-all.cc
	$(CPP) $(CPPFLAGS) -pthread -c $(GTEST_DIR)/src/gtest_main.cc
	ar -rv $@ gtest-all.o gtest-main.o

manual :
	$(CPP) $(CPPFLAGS) -pthread -c $(GTEST_DIR)/src/gtest-all.cc $(GTEST_DIR)/src/gtest_main.cc
	ar -rv libgtest.a gtest-all.o gtest_main.o
