
all : libscam.a

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

CPP = /usr/bin/g++
INC =  -I ../include
CPPFLAGS = -g -Wall $(INC)
SUBOBJ = env/*.o expr/*.o form/*.o input/*.o port/*.o prim/*.o util/*.o

libscam.a : $(OBJ) subs
	ar crf	$@ $(OBJ) $(SUBOBJ)

$(OBJ) : %.o : %.cpp
	$(CPP) $(CPPFLAGS) -c $<

$(DEP) : %.d : %.cpp
	$(CPP) $(CPPFLAGS)  -MM $< -o $*.d

clean :
	-@rm -f *.o *.d *.a
	make -C env clean
	make -C expr clean
	make -C form clean
	make -C input clean
	make -C port clean
	make -C prim clean
	make -C util clean

subs :
	make -C env all
	make -C expr all
	make -C form all
	make -C input all
	make -C port all
	make -C prim all
	make -C util all

include $(DEP)
