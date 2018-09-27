
all : libexpr.a

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

CPP = /usr/bin/g++ -g -std=c++11 -Wall
INCL = -I ../../include

libexpr.a : $(OBJ)
	ar crf	$@ $(OBJ)

$(OBJ) : %.o : %.cpp
	$(CPP) $(INCL) -c $<

$(DEP) : %.d : %.cpp
	$(CPP) $(INCL) -MM $< -o $*.d

clean :
	-@rm -f *.o *.d *.a

include $(DEP)
