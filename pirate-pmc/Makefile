PERL = /usr/bin/perl
BUILD = $(PERL) ../../../tools/build/dynpmc.pl
DESTDIR = ../../../runtime/parrot/dynext
LOAD_EXT = .so
PARROT = ../../parrot

PMCDIR = pmc


PMCS =  pyobject \
pyint \
pylong \
pyfloat \
pycomplex \
pyboolean \
pystring \
pynone \
pytuple \
pylist \
pydict \
pyclass \
pytype \
pyproxytype \
pyproxyclass \
pyfunc \
pynci \
pyboundmeth \
pyboundcall \
pyexception \
pyiter \
pymodule \
pyslice \
pystaticmeth \
pygen \
pybuiltin


all: $(PARROT) pmcs

pmcs:
	@cd $(PMCDIR) && $(BUILD) generate $(PMCS)
	@cd $(PMCDIR) && $(BUILD) compile $(PMCS)
	@cd $(PMCDIR) && $(BUILD) linklibs $(PMCS)
	@cd $(PMCDIR) && $(BUILD) copy "--destination=$(DESTDIR)" $(PMCS)

fulltest: testclass test testbench

test:
	perl t/harness t/basic/*.t

testbench:
	perl t/harness t/pie/b*.t

testclass:
	perl t/harness t/pmc/py*.t

clean:
	cd ./pmc; rm *.dump *.c *.h *.o *.bundle *.so;cp pyconsts.h.save pyconsts.h
