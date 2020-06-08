CC =gfortran
CFLAGS = -O3 -fno-range-check -ffree-line-length-none -w -fopenmp
LFLAGS = /usr/local/lib/liblapack.a /usr/local/lib/libblas.a /usr/local/lib/libnlopt.a
INCLUDES = -I/usr/local/include/

SRS1 = tools/prec.f90 \
       tools/kind_module.f90

SRS3 = tools/csv/csv_kinds.f90  tools/csv/csv_parameters.f90  \
       tools/csv/csv_utilities.f90  tools/csv/csv_zmodule.f90

SRS = $(SRS1) $(SRS3)

OBJ= $(patsubst tools/%.f90,%.o, \
     $(patsubst tools/csv/%.f90,%.o,$(SRS)))

SA1 = aux_routines/parameters.f90 \
      aux_routines/globals.f90 \
      aux_routines/globals_smm.f90 \
      aux_routines/process.f90

$(OBJ): $(SRS)
	$(CC) $(CFLAGS) $(SRS) -c

main: $(OBJ) $(SA1) main.f90
	$(CC) $^ -o main $(CFLAGS) $(LFLAGS) $(INCLUDES)

clean:
	rm -f *.o *.mod *~ $(DRT)*~ scripts/*~ aux_routines/*~ figure_scripts/*~ \
	fort.* *# main 

