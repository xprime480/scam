
all : testscam

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

CPP = /usr/bin/g++
INC = -I include -I ../include
CPPFLAGS =  -g -Wall $(INC)

testscam : $(OBJ) lib
	$(CPP) -o $@ $(OBJ) -L../src -lscam

$(OBJ) : %.o : %.cpp
	$(CPP) $(CPPFLAGS) -c $<

$(DEP) : %.d : %.cpp
	$(CPP) $(CPPFLAGS) -MM $< -o $*.d

lib :
	make -C ../src all

test : testscam
	@for X in `find . -name '*.mst'`; do ./testscam $$X; done

testu : testscam
	@./testscam `find . -name '*.mst'`

current : testscam
	@./testscam current.mst

clean :
	-@rm -f *.o *.d testscam

include $(DEP)

.PHONY : current testscam test
