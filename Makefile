#> monolis Makefile
FC     = mpif90
FFLAGS = -O2 -mtune=native -march=native -std=legacy

CC     = mpic++
CFLAGS = -O2

MOD_DIR  = -J ./include

# DEFAULT FLAGS
FLAG_MPI   = -DWITH_MPI
FLAG_METIS = -DWITH_METIS

METIS_DIR  = .
METIS_INC  = -I $(METIS_DIR)/include
METIS_LIB  = -L$(METIS_DIR)/lib -lmetis

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

	ifeq ($(findstring TEST, $(DFLAGS)), TEST)
		FLAG_TEST = -DTEST
	endif

	ifeq ($(findstring METIS64, $(DFLAGS)), METIS64)
		FLAG_METIS = -DWITH_METIS64
	endif
endif

INCLUDE  = -I /usr/include -I ./include $(METIS_INC)
LIBRARY  = $(METIS_LIB)
BIN_DIR  = ./bin
SRC_DIR  = ./src
OBJ_DIR  = ./obj
LIB_DIR  = ./lib
LIB_LIST = libgedatsu.a
GEDATSU_LIB = -L$(LIB_DIR) -lgedatsu
CPP      = -cpp $(FLAG_MPI) $(FLAG_METIS) $(FLAG_TEST) $(FLAG_DEBUG)

BIN_PART = gedatsu_partitioner

MAKE     = make
CD       = cd
RM       = rm -r
AR       = - ar ruv

LIBTARGET  = $(addprefix $(LIB_DIR)/, $(LIB_LIST))
#PARTTARGET = $(addprefix $(BIN_DIR)/, $(BIN_PART))

SRC_LIST_UTIL   = def_prm.f90 def_graph.f90 util.f90 io.f90
SRC_LIST_GRAPH  = graph.f90
#SRC_PART        = partitioner/partitioner.f90

SRC_ALL_LIST    = $(addprefix util/, $(SRC_LIST_UTIL))

SOURCES = $(addprefix $(SRC_DIR)/, $(SRC_ALL_LIST))
OBJS = $(subst $(SRC_DIR), $(OBJ_DIR), $(SOURCES:.f90=.o))

all: $(LIBTARGET)
#$(PARTTARGET)

$(LIBTARGET): $(OBJS)
	$(AR) $@ $(OBJS)

$(PARTTARGET): $(OBJS_PART)
	$(FC) $(FFLAGS) -o $@ $(OBJS_PART) $(MONOLIS_LIB) $(LIBRARY)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(INCLUDE) $(FLAG_METIS) -o $@ -c $<

clean:
	$(RM) $(OBJS) $(LIBTARGET) $(PARTTARGET) ./include/*.mod

distclean:
	$(RM) $(OBJS) $(LIBTARGET) $(PARTTARGET) /include/*.mod

sampleclean:

.PHONY: clean
