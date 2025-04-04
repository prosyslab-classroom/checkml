MAKE=@make
DUNE=@dune
LN=@ln -sf
RM=@rm
EXE=checkml

all:
	$(DUNE) build
	$(LN) _build/default/src/checkml.exe $(EXE)

test: all
	$(DUNE) test

promote:
	$(DUNE) promote

clean:
	$(DUNE) clean
	$(RM) -rf $(EXE)
