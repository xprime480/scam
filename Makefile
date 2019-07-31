
all : scam

clean :
	make -C src clean
	make -C integration clean
	make -C unit clean
	make -C scam clean

int :
	make -C integration test

sint :
	make -C scam test

unit :
	make -C unit test

scam :
	make -C scam

include $(DEP)

.PHONY : clean int sint unit scam
