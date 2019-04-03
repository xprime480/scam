
all : scam

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

CPP = /usr/bin/g++
INC = -I include -I ../include
CPPFLAGS =  -g -Wall $(INC)

scam : $(OBJ) lib
	$(CPP) -o $@ $(OBJ) -L../src -lscam

$(OBJ) : %.o : %.cpp
	$(CPP) $(CPPFLAGS) -c $<

$(DEP) : %.d : %.cpp
	$(CPP) $(CPPFLAGS) -MM $< -o $*.d

lib :
	make -C ../src all

test : testscam
	@./testscam `find . -name '*.mst'`

clean :
	-@rm -f *.o *.d scam

include $(DEP)
