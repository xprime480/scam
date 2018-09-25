
all : unittests

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

CPP = /usr/bin/g++ -g -std=c++11
INC = -I include -I ../include

unittests : $(OBJ) lib
	$(CPP) -o $@ $(OBJ) -L../src -lscam

$(OBJ) : %.o : %.cpp
	$(CPP) $(INC) -c $<

$(DEP) : %.d : %.cpp
	$(CPP) $(INC) -MM $< -o $*.d

lib :
	make -C ../src all

test : unittests
	@./unittests

clean :
	make -C ../src clean
	-@rm -f *.o *.d unittests

include $(DEP)
