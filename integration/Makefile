
all : testscam

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

CPP = /usr/bin/g++ -g -std=c++11 -Wall
INC = -I include -I ../include

testscam : $(OBJ) lib
	$(CPP) -o $@ $(OBJ) -L../src -lscam

$(OBJ) : %.o : %.cpp
	$(CPP) $(INC) -c $<

$(DEP) : %.d : %.cpp
	$(CPP) $(INC) -MM $< -o $*.d

lib :
	make -C ../src all

test : testscam
	@./testscam `find . -name '*.mst'`

current : testscam
	@./testscheme current.mst

clean :
	-@rm -f *.o *.d testscam

include $(DEP)
