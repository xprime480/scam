
all : scam

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

CPP = /usr/bin/g++
INC = -I include -I ../include
GMP = -L/usr/local/lib -lgmpxx -lgmp
CPPFLAGS = -g -Wall $(INC) $(GMP)

scam : $(OBJ) lib
	$(CPP) -g -o $@ $(OBJ) -L../src -lscam $(GMP)

$(OBJ) : %.o : %.cpp
	$(CPP) $(CPPFLAGS) -c $<

$(DEP) : %.d : %.cpp
	$(CPP) $(CPPFLAGS) -MM $< -o $*.d

lib :
	make -C ../src all

test : scam
	@./scamtest.sh

qtest :
	@./scamtest.sh

current :
	./scam -t $(CURRENT_TEST)

clean :
	-@rm -f *.o *.d scam

include $(DEP)
