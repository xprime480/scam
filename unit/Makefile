
all : unittests

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

GTEST_DIR=../../googletest/googletest
CPP = /usr/bin/g++ -g -std=c++11 -Wall
INC = -I include -I ../include -isystem $(GTEST_DIR)/include -I$(GTEST_DIR)

unittests : $(OBJ) lib gmocklib
	$(CPP) -o $@ $(OBJ) -L../src -lscam -Lgmock -lgtest -lpthread

$(OBJ) : %.o : %.cpp
	$(CPP) $(INC) -c $<

$(DEP) : %.d : %.cpp
	$(CPP) $(INC) -MM $< -o $*.d

lib :
	make -C ../src all

gmocklib :
	:

do_it_manually :
	make -C gmock all

## Until the memory issue is fixed, run the tests in batches
##
test : unittests
	@./unittests --gtest_filter=ComparisonTest.*
	@./unittests --gtest_filter=EnvTest.*
	@./unittests --gtest_filter=ExpressionTest.*
	@./unittests --gtest_filter=LogicTest.*
	@./unittests --gtest_filter=MathTest.*
	@./unittests --gtest_filter=ParserTest.*
	@./unittests --gtest_filter=TokenizerTest.*
	@./unittests --gtest_filter=TrampolineTest.*

clean :
	make -C ../src clean
	-@rm -f *.o *.d unittests

include $(DEP)

.PHONY : test
