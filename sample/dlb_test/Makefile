
FC = mpif90
FFLAGS = -O2

TARGET = a.out
OBJS = main.o

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) -o $@ $(OBJS) -L../../lib -lgedatsu -lparmetis -lmetis -lGKlib

%.o: %.f90
	$(FC) $(FFLAGS) -I../../include -o $@ -c $<

clean:
	rm a.out
	rm *.o

.PHONY: clean
