
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

test : unittests
	@./unittests

by_module : unittests
	@./unittests --gtest_filter=TrampolineTest.*
	@./unittests --gtest_filter=EnvTest.*
	@./unittests --gtest_filter=TokenizerTest.*
	@./unittests --gtest_filter=ParserTest.*
	@./unittests --gtest_filter=ExpressionTest.*
	@./unittests --gtest_filter=ComparisonTest.*
	@./unittests --gtest_filter=EqualTest.*
	@./unittests --gtest_filter=PredicateTest.*
	@./unittests --gtest_filter=LogicTest.*
	@./unittests --gtest_filter=MathTest.*
	@./unittests --gtest_filter=ClosureTest.*
	@./unittests --gtest_filter=LetTest.*
	@./unittests --gtest_filter=ListTest.*
	@./unittests --gtest_filter=EvalTest.*
	@./unittests --gtest_filter=ClassTest.*
	@./unittests --gtest_filter=ContinuationTest.*
	@./unittests --gtest_filter=SystemTest.*
	@./unittests --gtest_filter=PreludeTest.*

clean :
	make -C ../src clean
	-@rm -f *.o *.d unittests

include $(DEP)

.PHONY : test
