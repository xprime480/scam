
all : scam

clean :
	make -C src clean
	make -C unit clean
	make -C scam clean

int :
	make -C scam test

unit :
	make -C unit test

scam :
	make -C scam

current : scam
	make -C scam current

include $(DEP)

.PHONY : clean int unit scam
