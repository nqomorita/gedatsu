#> gedatsu Makefile

##> compiler setting
FC     = mpif90
FFLAGS = -fPIC -O2 -mtune=native -march=native -std=legacy -Wno-missing-include-dirs
CC     = mpicc
CFLAGS = -fPIC -O2

##> directory setting
MOD_DIR = -J ./include
INCLUDE = -I /usr/include -I ./include -I ./submodule/monolis_utils/include
USE_LIB = -L./lib -lgedatsu -L./submodule/monolis_utils/lib -lmonolis_utils -lmetis
BIN_DIR = ./bin
SRC_DIR = ./src
TST_DIR = ./src_test
OBJ_DIR = ./obj
LIB_DIR = ./lib
WRAP_DIR= ./wrapper
TST_WRAP_DIR = ./wrapper_test
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
		FFLAGS  = -fPIC -O2 -std=legacy -fbounds-check -fbacktrace -Wuninitialized -ffpe-trap=invalid,zero,overflow -Wno-missing-include-dirs -Wall
		CFLAGS  = -fPIC -O2 -g -ggdb
	endif

	ifeq ($(findstring INTEL, $(DFLAGS)), INTEL)
		FC      = mpiifort
		FFLAGS  = -fPIC -O2 -align array64byte  -nofor-main
		CC      = mpiicc
		CFLAGS  = -fPIC -O2 -no-multibyte-chars
		MOD_DIR = -module ./include
	endif

	ifeq ($(findstring INT64, $(DFLAGS)), INT64)
		FLAG_INT64 = -DINT64
	endif

	ifeq ($(findstring SUBMODULE, $(DFLAGS)), SUBMODULE)
		INCLUDE = -I /usr/include -I ./include -I ../monolis_utils/include -I ../../include
		USE_LIB = -L./lib -lgedatsu -L../monolis_utils/lib -lmonolis_utils -L../../lib -lmetis
	endif
endif

##> other commands
MAKE = make
CD   = cd
CP   = cp
RM   = rm -rf
AR   = - ar ruv

##> **********
##> target (1)
LIB_TARGET = $(LIB_DIR)/$(LIBRARY)

##> source file define
SRC_DEF = \
  def_graph.f90 \
  def_dlb.f90

SRC_WRAP = \
  wrapper_metis.f90 \
  wrapper_parmetis.f90

SRC_GRAPH = \
  graph_handler.f90 \
  graph_convert.f90 \
  graph_part.f90

SRC_DLB = \
  graph_repart.f90 \
  dlb_comm_nodal.f90 \
  dlb_comm_conn.f90 \
  dlb_handler.f90

##> C wrapper section
SRC_GRAPH_C = \
  gedatsu_graph_convert_c.c

SRC_ALL_C = \
$(addprefix graph/, $(SRC_GRAPH_C))

##> all targes
SRC_ALL = \
$(addprefix define/, $(SRC_DEF)) \
$(addprefix wrapper/, $(SRC_WRAP)) \
$(addprefix graph/, $(SRC_GRAPH)) \
$(addprefix dlb/, $(SRC_DLB))

##> lib objs
LIB_SOURCES = \
$(addprefix $(SRC_DIR)/,  $(SRC_ALL)) \
$(addprefix $(WRAP_DIR)/, $(SRC_ALL_C)) \
./src/gedatsu.f90
LIB_OBJSt   = $(subst $(SRC_DIR), $(OBJ_DIR), $(LIB_SOURCES:.f90=.o))
LIB_OBJS    = $(subst $(WRAP_DIR), $(OBJ_DIR), $(LIB_OBJSt:.c=.o))

##> **********
##> target (2) test for fotran
TEST_TARGET = $(TST_DIR)/gedatsu_test

##> lib objs
TST_SRC_ALL = $(SRC_ALL) driver/driver.f90 driver/driver_c.f90 gedatsu.f90
TST_SOURCES = $(addprefix $(TST_DIR)/, $(TST_SRC_ALL))
TST_OBJSt   = $(subst $(TST_DIR), $(OBJ_DIR), $(TST_SOURCES:.f90=_test.o))
TST_OBJS    = $(TST_OBJSt:.c=_test.o)

##> **********
##> target (3) test for fotran
TEST_C_TARGET = $(TST_WRAP_DIR)/gedatsu_c_test

##> lib objs
SRC_GRAPH_C_TEST = \
gedatsu_graph_convert_c_test.c

SRC_ALL_C_TEST = \
$(addprefix graph/, $(SRC_GRAPH_C_TEST))

TST_SRC_C_ALL = $(SRC_ALL_C_TEST) gedatsu_c_test.c
TST_C_SOURCES = $(addprefix $(TST_WRAP_DIR)/, $(TST_SRC_C_ALL))
TST_C_OBJS    = $(subst $(TST_WRAP_DIR), $(OBJ_DIR), $(TST_C_SOURCES:.c=.o))

##> **********
##> target (4) driver
DRIVE1 = $(BIN_DIR)/gedatsu_simple_mesh2graph_convertor
DRIVE2 = $(BIN_DIR)/gedatsu_bc_partitioner_R
DRIVE3 = $(BIN_DIR)/gedatsu_bc_partitioner_C
DRIVE4 = $(BIN_DIR)/gedatsu_connectivity_graph_partitioner
DRIVE5 = $(BIN_DIR)/gedatsu_nodal_graph_partitioner
DRIVE6 = $(BIN_DIR)/gedatsu_dist_val_partitioner_I
DRIVE7 = $(BIN_DIR)/gedatsu_dist_val_partitioner_R
DRIVE8 = $(BIN_DIR)/gedatsu_dist_val_partitioner_C
DRIVE9 = $(BIN_DIR)/gedatsu_simple_mesh_partitioner

DRV_OBJS1 = $(DRV_OBJSt:.c=.o) ./obj/convert_simple_mesh2graph.o
DRV_OBJS2 = $(DRV_OBJSt:.c=.o) ./obj/part_bc_R.o
DRV_OBJS3 = $(DRV_OBJSt:.c=.o) ./obj/part_bc_C.o
DRV_OBJS4 = $(DRV_OBJSt:.c=.o) ./obj/part_conn_graph.o
DRV_OBJS5 = $(DRV_OBJSt:.c=.o) ./obj/part_nodal_graph.o
DRV_OBJS6 = $(DRV_OBJSt:.c=.o) ./obj/part_dist_val_I.o
DRV_OBJS7 = $(DRV_OBJSt:.c=.o) ./obj/part_dist_val_R.o
DRV_OBJS8 = $(DRV_OBJSt:.c=.o) ./obj/part_dist_val_C.o
DRV_OBJS9 = $(DRV_OBJSt:.c=.o) ./obj/part_simple_mesh.o

##> target
all: \
	cp_header \
	$(LIB_TARGET) \
	$(TEST_TARGET) \
	$(TEST_C_TARGET) \
	$(DRIVE1) \
	$(DRIVE2) \
	$(DRIVE3) \
	$(DRIVE4) \
	$(DRIVE5) \
	$(DRIVE6) \
	$(DRIVE7) \
	$(DRIVE8) \
	$(DRIVE9)

lib: \
	cp_header \
	$(LIB_TARGET)

$(LIB_TARGET): $(LIB_OBJS)
	$(AR) $@ $(LIB_OBJS)

$(TEST_TARGET): $(TST_OBJS)
	$(FC) $(FFLAGS) -o $@ $(TST_OBJS) $(USE_LIB)

$(TEST_C_TARGET): $(TST_C_OBJS)
	$(FC) $(FFLAGS) $(INCLUDE) -o $@ $(TST_C_OBJS) $(USE_LIB)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(TST_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(DRV_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(WRAP_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(WRAP_DIR)/%.c
	$(CC) $(CFLAGS) $(INCLUDE) -o $@ -c $<

$(OBJ_DIR)/%.o: $(TST_WRAP_DIR)/%.c
	$(CC) $(CFLAGS) $(INCLUDE) -o $@ -c $<

$(DRIVE1): $(DRV_OBJS1)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS1) $(USE_LIB)

$(DRIVE2): $(DRV_OBJS2)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS2) $(USE_LIB)

$(DRIVE3): $(DRV_OBJS3)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS3) $(USE_LIB)

$(DRIVE4): $(DRV_OBJS4)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS4) $(USE_LIB)

$(DRIVE5): $(DRV_OBJS5)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS5) $(USE_LIB)

$(DRIVE6): $(DRV_OBJS6)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS6) $(USE_LIB)

$(DRIVE7): $(DRV_OBJS7)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS7) $(USE_LIB)

$(DRIVE8): $(DRV_OBJS8)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS8) $(USE_LIB)

$(DRIVE9): $(DRV_OBJS9)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS9) $(USE_LIB)

cp_header:
	$(CP) ./wrapper/graph/gedatsu_graph_convert_c.h ./include/
	$(CP) ./wrapper/gedatsu.h ./include/

clean:
	$(RM) \
	$(LIB_OBJS) \
	$(TST_OBJS) \
	$(TST_C_OBJS) \
	$(DRV_OBJS1) \
	$(DRV_OBJS2) \
	$(DRV_OBJS3) \
	$(DRV_OBJS4) \
	$(DRV_OBJS5) \
	$(DRV_OBJS6) \
	$(DRV_OBJS7) \
	$(DRV_OBJS8) \
	$(DRV_OBJS9) \
	$(LIB_TARGET) \
	$(TEST_TARGET) \
	$(TEST_C_TARGET) \
	$(DRIVE1) \
	$(DRIVE2) \
	$(DRIVE3) \
	$(DRIVE4) \
	$(DRIVE5) \
	$(DRIVE6) \
	$(DRIVE7) \
	$(DRIVE8) \
	$(DRIVE9) \
	./include/*.h \
	./include/*.mod \
	./bin/*

.PHONY: clean
