INCDIRS = -I /usr/include/python2.2  -I /usr/local/include -I /usr/include
LIBDIRS = -L /usr/lib/python2.2/config -L/usr/local/lib -L/usr/lib 


LIBS=           -lnsl -ldl -lreadline -ltermcap -lieee -lpthread -lutil 
LDFLAGS=        -Xlinker -export-dynamic
LDSHARED=	-shared -L/usr/local/lib
SYSLIBS=        -lm
LIBPYTHON= -lpython2.2
ALLLIBS=        $(LIBPYTHON) $(MODLIBS) $(LIBS) $(SYSLIBS)

all: clean spike

spike: 
	gcc $(LDFLAGS) $(INCDIRS) $(LIBDIRS)  strspike.c $(ALLLIBS) -o strspike
	echo ##################################################
	./strspike

clean:
	rm -f strspike
