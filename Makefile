CC=ocamlopt
CFLAGS=-w -40-8
LDFLAGS=-pp camlp4o
EXEC=calc

SRCS=data.ml syntax.ml exec.ml main.ml

all: $(EXEC)

$(EXEC): $(SRCS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

.PHONY: clean

clean:
	rm -f *.cm*
	rm -f *.o

mrproper: clean
	rm -f $(EXEC)
