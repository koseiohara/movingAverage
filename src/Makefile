EXE = EXE
OBJS = NaNchecker.o fileio.o globals.o namelist.o debugger.o movave_filter.o movave.o main.o

FC = ifort
FLAGS = -warn all -O0 -assume byterecl -convert little_endian

%.o : %.f90
	${FC} -c $< ${FLAGS}

all : ${EXE}

${EXE} : ${OBJS}
	${FC} -o $@ $^


.PHONY : clean re run

clean :
	@ rm -fv *.o *.mod ${EXE}

re : clean all

run :
	./${EXE}

