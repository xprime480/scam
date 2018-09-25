
all : libscam.a

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

CPP = /usr/bin/g++ -g -std=c++11

libscam.a : $(OBJ) subs
	ar crf	$@ $(OBJ) input/*.o output/*.o

$(OBJ) : %.o : %.cpp
	$(CPP) -I ../include -c $<

$(DEP) : %.d : %.cpp
	$(CPP) -I ../include -MM $< -o $*.d

clean :
	-@rm -f *.o *.d *.a
	make -C input clean
	make -C output clean

subs :
	make -C input all
	make -C output all

include $(DEP)
