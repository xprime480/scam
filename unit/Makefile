
all : unittests

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

GTEST_DIR=../../googletest/googletest
CPP = /usr/bin/g++
INC = -I include -I ../include -isystem $(GTEST_DIR)/include -I$(GTEST_DIR)
CPPFLAGS = -g -Wall $(INC)
TEST_EXE = ./unittests

exe : lib gmocklib qexe
	:

$(OBJ) : %.o : %.cpp
	$(CPP) $(CPPFLAGS) -c $<

$(DEP) : %.d : %.cpp
	$(CPP) $(CPPFLAGS) -MM $< -o $*.d

lib :
	make -C ../src all

gmocklib :
	:

qexe : $(OBJ)
	$(CPP) -o $(TEST_EXE) $(OBJ) -L../src -lscam -Lgmock -lgtest -lpthread
	touch $@

do_it_manually :
	make -C gmock all

#TEST_FILTER = --gtest_filter=MemoryTest.*

qtest : qexe
	@$(TEST_EXE) $(TEST_FILTER) --gtest_print_time=0

test : exe qtest
	:

by_module : exe
	@./$(TEST_EXE) --gtest_filter=TrampolineTest.*
	@./$(TEST_EXE) --gtest_filter=EnvTest.*
	@./$(TEST_EXE) --gtest_filter=TokenizerTest.*
	@./$(TEST_EXE) --gtest_filter=ParserTest.*
	@./$(TEST_EXE) --gtest_filter=ExpressionTest.*
	@./$(TEST_EXE) --gtest_filter=ComparisonTest.*
	@./$(TEST_EXE) --gtest_filter=EqualTest.*
	@./$(TEST_EXE) --gtest_filter=PredicateTest.*
	@./$(TEST_EXE) --gtest_filter=LogicTest.*
	@./$(TEST_EXE) --gtest_filter=MathTest.*
	@./$(TEST_EXE) --gtest_filter=ClosureTest.*
	@./$(TEST_EXE) --gtest_filter=LetTest.*
	@./$(TEST_EXE) --gtest_filter=ListTest.*
	@./$(TEST_EXE) --gtest_filter=EvalTest.*
	@./$(TEST_EXE) --gtest_filter=ClassTest.*
	@./$(TEST_EXE) --gtest_filter=ContinuationTest.*
	@./$(TEST_EXE) --gtest_filter=SystemTest.*
	@./$(TEST_EXE) --gtest_filter=PreludeTest.*
	@./$(TEST_EXE) --gtest_filter=MemoryTest.*

clean :
	make -C ../src clean
	-@rm -f *.o *.d $(TEST_EXE) qexe

include $(DEP)

.PHONY : test qtest qexe
