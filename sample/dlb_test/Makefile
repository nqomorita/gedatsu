
FC = mpif90
FFLAGS = -O2

TARGET = a.out
OBJS = main.o

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) -o $@ $(OBJS) -L../../lib -lgedatsu -L../../submodule/monolis_utils/lib -lmonolis_utils -lparmetis -lmetis

%.o: %.f90
	$(FC) $(FFLAGS) -I../../include -I../../submodule/monolis_utils/include -o $@ -c $<

clean:
	rm a.out
	rm *.o

.PHONY: clean
