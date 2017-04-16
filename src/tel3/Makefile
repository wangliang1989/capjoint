
FC=/opt/g95/bin/g95
FC=gfortran
PROG= teles
SRCS=getgeomnew.f \
	sub.bodyw3.f \
	sub.cfft.f \
	sub.clear.f \
	sub.cnvr.f \
	sub.cnvrsh.f \
	sub.instg.f \
	sub.prod.f \
	sub.qf.f \
	sub.radp3.f \
	sub.refl.f \
	sub.reflsh.f
OBJS=${SRCS:%.f=%.o} sacio.o
LIBS= -L/usr/X11R6/lib -L/opt/sac-101.3b/lib
FFLAGS = -C -O -g


teleseis3: $(OBJS) teleseis3.f
	$(FC) $(FFLAGS) -o $(PROG) teleseis3.f $(OBJS) $(LIBS)

clean:
	\rm -f $(OBJS) *~ core
