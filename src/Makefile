
all : libscam.a

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

CPP = /usr/bin/g++ -g -std=c++11 -Wall
SUBOBJ = input/*.o output/*.o expr/*.o

libscam.a : $(OBJ) subs
	ar crf	$@ $(OBJ) $(SUBOBJ)

$(OBJ) : %.o : %.cpp
	$(CPP) -I ../include -c $<

$(DEP) : %.d : %.cpp
	$(CPP) -I ../include -MM $< -o $*.d

clean :
	-@rm -f *.o *.d *.a
	make -C expr clean
	make -C input clean
	make -C output clean

subs :
	make -C expr all
	make -C input all
	make -C output all

include $(DEP)
