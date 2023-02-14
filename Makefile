#> gedatsu Makefile

##> compiler setting
FC     = mpif90
#FFLAGS = -fPIC -O2 -mtune=native -march=native -std=legacy -Wno-missing-include-dirs
FFLAGS = -fPIC -O2 -std=legacy -fbounds-check -fbacktrace -Wuninitialized -ffpe-trap=invalid,zero,overflow -Wno-missing-include-dirs
CC     = mpicc
CFLAGS = -fPIC -O2

##> directory setting
MOD_DIR = -J ./include
INCLUDE = -I /usr/include -I ./include -I ../monolis_utils/include
BIN_DIR = ./bin
SRC_DIR = ./src
OBJ_DIR = ./obj
LIB_DIR = ./lib
TST_DIR = ./test
DRV_DIR = ./driver
LIBRARY = libgedatsu.a
CPP     = -cpp $(FLAG_DEBUG)

##> option setting
ifdef FLAGS
	comma:= ,
	empty:=
	space:= $(empty) $(empty)
	DFLAGS = $(subst $(comma), $(space), $(FLAGS))

	ifeq ($(findstring DEBUG, $(DFLAGS)), DEBUG)
		FFLAGS  = -fPIC -O2 -std=legacy -fbounds-check -fbacktrace -Wuninitialized -ffpe-trap=invalid,zero,overflow -Wno-missing-include-dirs
	endif

	ifeq ($(findstring INTEL, $(DFLAGS)), INTEL)
		FC      = mpiifort
		FFLAGS  = -fPIC -O2 -align array64byte
		CC      = mpiicc
		CFLAGS  = -fPIC -O2 -no-multibyte-chars
		MOD_DIR = -module ./include
	endif

	ifeq ($(findstring INT64, $(DFLAGS)), INT64)
		FLAG_INT64 = -DINT64
	endif
endif

##> other commands
MAKE = make
CD   = cd
RM   = rm -rf
AR   = - ar ruv

##> **********
##> target (1)
LIB_TARGET = $(LIB_DIR)/$(LIBRARY)

##> source file define
SRC_UTIL = \
  def_graph.f90 \
  def_dlb.f90 \
  wrapper_metis.f90 \
  wrapper_parmetis.f90

SRC_GRAPH = \
  graph_handler.f90 \
  graph_convert.f90 \
  graph_part.f90

SRC_DLB = \
  graph_repart.f90 \
  dlb_comm.f90 \
  dlb_handler.f90

SRC_ALL = \
$(addprefix util/, $(SRC_UTIL)) \
$(addprefix graph/, $(SRC_GRAPH)) \
$(addprefix dlb/, $(SRC_DLB)) \
gedatsu.f90

##> lib objs
LIB_SOURCES = $(addprefix $(SRC_DIR)/, $(SRC_ALL))
LIB_OBJSt   = $(subst $(SRC_DIR), $(OBJ_DIR), $(LIB_SOURCES:.f90=.o))
LIB_OBJS    = $(LIB_OBJSt:.c=.o)

##> **********
##> target (2)
TEST_TARGET = $(TST_DIR)/gedatsu_test

##> lib objs
TST_SOURCES = $(addprefix $(TST_DIR)/, $(SRC_ALL))
TST_OBJSt   = $(subst $(TST_DIR), $(OBJ_DIR), $(TST_SOURCES:.f90=_test.o))
TST_OBJS    = $(TST_OBJSt:.c=_test.o)

##> **********
##> target (3)
DRIVE1 = $(BIN_DIR)/gedatsu_graph_partitioner

#SRC_DRIVE = \
driver_util.f90 \
extract_util.f90 \
refiner_util.f90

#DRV_SOURCES = $(addprefix $(DRV_DIR)/, $(SRC_DRIVE))
#DRV_OBJSt   = $(subst $(DRV_DIR), $(OBJ_DIR), $(DRV_SOURCES:.f90=.o))

DRV_OBJS1   = $(DRV_OBJSt:.c=.o) ./obj/gedatsu_graph_partitioner.o

##> target
all: \
	$(LIB_TARGET) \
	$(TEST_TARGET)
#$(DRIVE1)

lib: \
	$(LIB_TARGET)

$(LIB_TARGET): $(LIB_OBJS)
	$(AR) $@ $(LIB_OBJS)

$(TEST_TARGET): $(TST_OBJS)
	$(FC) $(FFLAGS) -o $@ $(TST_OBJS) -L./lib -lgedatsu -L../monolis_utils/lib -lmonolis_utils -lmetis -lGKlib

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(CPP) $(INCLUDE) -o $@ -c $<

$(OBJ_DIR)/%.o: $(TST_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(TST_DIR)/%.c
	$(CC) $(CFLAGS) $(CPP) $(INCLUDE) -o $@ -c $<

$(OBJ_DIR)/%.o: $(DRV_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(DRV_DIR)/%.c
	$(CC) $(CFLAGS) $(CPP) $(INCLUDE) -o $@ -c $<

$(DRIVE1): $(DRV_OBJS1)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS1) -L./lib -lgedatsu -L../monolis_utils/lib -lmonolis_utils -lmetis -lGKlib

clean:
	$(RM) \
	$(LIB_OBJS) \
	$(TST_OBJS) \
	$(DRV_OBJS1) \
	$(LIB_TARGET) \
	$(TEST_TARGET) \
	$(DRIVE1) \
	./include/*.mod \
	./bin/*

.PHONY: clean
