
GTEST_DIR= ../../../googletest/googletest
CPP = /usr/bin/g++ -g -std=c++11 -Wall
INC = -isystem $(GTEST_DIR)/include -I$(GTEST_DIR)

all : libgtest.a

libgtest.a : $(GTEST_DIR)/src/gtest-all.cc $(GTEST_DIR)/src/gtest_main.cc
	$(CPP) $(INC) -pthread -c $<
	ar -rv $@ gtest-all.o

manual :
	$(CPP) $(INC) -pthread -c $(GTEST_DIR)/src/gtest-all.cc $(GTEST_DIR)/src/gtest_main.cc
	ar -rv libgtest.a gtest-all.o gtest_main.o
