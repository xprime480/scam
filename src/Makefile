
all : libscam.a

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

CPP = /usr/bin/g++ -g -std=c++11

libscam.a : $(OBJ)
	ar crf	$@ $(OBJ)

$(OBJ) : %.o : %.cpp
	$(CPP) -I ../include -c $<

$(DEP) : %.d : %.cpp
	$(CPP) -I ../include -MM $< -o $*.d

clean :
	-@rm -f *.o *.d *.a

include $(DEP)
