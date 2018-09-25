
all : 

clean :
	make -C src clean
	make -C integration clean
	make -C unit clean

int :
	make -C integration test

unit :
	make -C unit test

include $(DEP)

.PHONY : clean int unit
