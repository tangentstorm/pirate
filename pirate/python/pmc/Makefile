#
# this is based on parrot's generated dynclasses/Makefile:
# @TODO: can we have parrot generate for us too?
#

LD = gcc
PERL = /usr/bin/perl
RM_F = rm -f
SO = .so

LDFLAGS=        -Xlinker -export-dynamic
LDSHARED=	-shared -L/usr/local/lib

## you want to change these:

PARROT_SRC_DIR = /home/sabren/parrot
RUNTIME_DIR = ../../runtime

#PYTHON_INCS = -I/usr/include/python2.2  -I/usr/local/include -I/usr/include
#PYTHON_LIBS = -L/usr/lib/python2.2/config -L/usr/local/lib -L/usr/lib 
PYTHON_INCS = -I/home/sabren/python/dist/src/ -I/home/sabren/python/dist/src/Include  -I/usr/local/include -I/usr/include
PYTHON_LIBS = -L/home/sabren/python/dist/src/ -L/usr/local/lib -L/usr/lib 
PYTHON_REQS = -lnsl -ldl -lreadline -ltermcap -lieee -lpthread -lutil -lm
LIBPYTHON   = -lpython2.4

PARROT_INCS = -I$(PARROT_SRC_DIR)/include -I$(PARROT_SRC_DIR)/classes
PARROT_LIBS = -L$(PARROT_SRC_DIR)/blib/lib
LIBPARROT   = -lparrot

all: clean pmcs test

# add your dynamic pmcs here:
pmcs: piobject$(SO) pisequence$(SO) pistring$(SO)

.SUFFIXES: .pmc .c $(SO)
.PRECIOUS: .c

%.c  : %.pmc
	$(PERL) $(PARROT_SRC_DIR)/classes/pmc2c2.pl --dump $<
	$(PERL) $(PARROT_SRC_DIR)/classes/pmc2c2.pl --c $<

%$(SO) : %.c
	$(LD) $(LDFLAGS) $(LDSHARED) \
	      $(PARROT_INCS) $(PARROT_LIBS) \
	      $(PYTHON_INCS) $(PYTHON_LIBS) $(PYTHON_REQS) \
              $(LIBPYTHON) $(LIBPARROT) -o $@ $<
	cp $< $<.bak
	$(PERL) -MFile::Copy=cp -e 'cp q|$@|, q|$(RUNTIME_DIR)/parrot/dynext/$@|'

clean :
	$(RM_F) *.c *.h *$(SO) *.dump

test :
	pushd ../../; parrot python/pmc/piobject.imc; popd
