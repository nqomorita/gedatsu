#> gedatsu Makefile
FC     = mpif90
#FFLAGS = -O2 -std=legacy -mtune=native -march=native
FFLAGS  = -O2 -std=legacy -fbounds-check -fbacktrace -Wall -ffpe-trap=invalid,zero,overflow

CC     = mpic++
CFLAGS = -O2

MAKE     = make
CD       = cd
RM       = rm -r
AR       = - ar ruv

#> library setting
METIS_DIR  = .
METIS_INC  = -I $(METIS_DIR)/include
METIS_LIB  = -L$(METIS_DIR)/lib -lmetis -lGKlib

PARMETIS_DIR  = .
PARMETIS_INC  = -I $(METIS_DIR)/include
PARMETIS_LIB  = -L$(METIS_DIR)/lib -lparmetis

#> option setting
ifdef FLAGS
	comma:= ,
	empty:=
	space:= $(empty) $(empty)
	DFLAGS = $(subst $(comma), $(space), $(FLAGS))

	ifeq ($(findstring DEBUG, $(DFLAGS)), DEBUG)
		FFLAGS  = -O2 -std=legacy -fbounds-check -fbacktrace -Wuninitialized -ffpe-trap=invalid,zero,overflow
	endif

	ifeq ($(findstring INTEL, $(DFLAGS)), INTEL)
		FC      = mpiifort
		FFLAGS  = -O2 -align array64byte
		CC      = mpiicpc
		CFLAGS  = -O2
		MOD_DIR = -module ./include
	endif

	ifeq ($(findstring INT64, $(DFLAGS)), INT64)
		FLAG_INT64 = -DINT64
	endif
endif

#> compilation directories
INCLUDE  = -I /usr/include -I ./include $(METIS_INC)
BIN_DIR  = ./bin
SRC_DIR  = ./src
OBJ_DIR  = ./obj
LIB_DIR  = ./lib
MOD_DIR  = -J ./include
LIB_LIST = libgedatsu.a
GEDATSU_LIB = -L$(LIB_DIR) -lgedatsu $(METIS_LIB)
CPP      = -cpp $(FLAG_INT64)

#> bin target
PRT_G_TGT = $(BIN_DIR)/gedatsu_graph_partitioner

#> library target
LIB_TGT = \
$(addprefix $(LIB_DIR)/, $(LIB_LIST))

SRC_LIST_UTIL1 = \
def_prm.f90 \

SRC_LIST_UTIL2 = \
util.f90 \
alloc.f90 \
def_graph.f90 \
std.f90 \
wrapper_metis.f90 \
wrapper_parmetis.f90 \
io_file_name.f90 \
io.f90 \
io_arg.f90

SRC_LIST_MPI = \
mpi_util.f90

SRC_LIST_GRAPH = \
graph_handler.f90 \
graph_comm.f90 \
graph_convert.f90 \
graph_part.f90

SRC_LIST_DLB = \
graph_repart.f90

SRC_ALL_LIST = \
$(addprefix util/, $(SRC_LIST_UTIL1)) \
$(addprefix mpi/, $(SRC_LIST_MPI)) \
$(addprefix util/, $(SRC_LIST_UTIL2)) \
$(addprefix graph/, $(SRC_LIST_GRAPH)) \
$(addprefix dlb/, $(SRC_LIST_DLB)) \
main/gedatsu.f90

SOURCES = $(addprefix $(SRC_DIR)/, $(SRC_ALL_LIST))
OBJS = $(subst $(SRC_DIR), $(OBJ_DIR), $(SOURCES:.f90=.o))

#> compilation
all: $(LIB_TGT) $(PRT_G_TGT)

$(LIB_TGT): $(OBJS)
	$(AR) $@ $(OBJS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(INCLUDE) $(FLAG_METIS) -o $@ -c $<

$(PRT_G_TGT): driver/gedatsu_graph_partitioner.f90
	$(FC) $(FFLAGS) $(INCLUDE) -o $@ $< $(GEDATSU_LIB)

#> clean
clean:
	$(RM) $(OBJS) $(LIB_TGT) ./include/*.mod \
	$(PRT_G_TGT)

distclean:
	$(RM) $(OBJS) $(LIB_TGT) /include/*.mod \
	$(PRT_G_TGT)

binclean:
	$(RM) $(PRT_G_TGT)

.PHONY: clean
