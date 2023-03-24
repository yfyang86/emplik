#include <math.h>
#include <R.h>
#include <Rconfig.h>
#include <Rdefines.h>
#include <float.h>

#include "foo.h"

#include <R_ext/Applic.h>

#define EPSILON DBL_EPSILON

/* R_zeroin2() is faster for "expensive" f(), in those typical cases where
 *             f(ax) and f(bx) are available anyway : */

 /* double lamfunC(double lambda,double * x, double mu,double * wt,double allw,int L); */

static inline double lamfunC(double lambda,double * x, double mu,double * wt,double allw,int L){
int i=0; double re=0.;
for (;i<L;i++) re+=wt[i]*(x[i]-mu)/(allw+lambda*(x[i]-mu));
return(re);
}

static double R_zeroin2surv(			/* An estimate of the root */
    double ax,				/* Left border | of the range	*/
    double bx,				/* Right border| the root is seeked*/
    double *Tol,			/* Acceptable tolerance		*/
    int *Maxit,				/* Max # of iterations */
    double * x, double mu,double * wt,double allw,int L /*par of f*/
    )				
{
    double a,b,c, fc;			/* Abscissae, descr. see above,  f(c) */
    double tol;
    int maxit;
    
    double fa=lamfunC(ax,x,mu,wt,allw,L), fb= lamfunC(bx,x,mu,wt,allw,L);		/* f(a), f(b) */
  
    a = ax;  b = bx;
    c = a;   fc = fa;
    maxit = *Maxit + 1; tol = * Tol;

    /* First test if we have found a root at an endpoint */
    if(fa == 0.0) {
	*Tol = 0.0;
	*Maxit = 0;
	return a;
    }
    if(fb ==  0.0) {
	*Tol = 0.0;
	*Maxit = 0;
	return b;
    }

    while(maxit--)		/* Main iteration loop	*/
    {
	double prev_step = b-a;		/* Distance from the last but one
					   to the last approximation	*/
	double tol_act;			/* Actual tolerance		*/
	double p;			/* Interpolation step is calcu- */
	double q;			/* lated in the form p/q; divi-
					 * sion operations is delayed
					 * until the last moment	*/
	double new_step;		/* Step at this iteration	*/

	if( fabs(fc) < fabs(fb) )
	{				/* Swap data for b to be the	*/
	    a = b;  b = c;  c = a;	/* best approximation		*/
	    fa=fb;  fb=fc;  fc=fa;
	}
	tol_act = 2*EPSILON*fabs(b) + tol/2;
	new_step = (c-b)/2;

	if( fabs(new_step) <= tol_act || fb == (double)0 )
	{
	    *Maxit -= maxit;
	    *Tol = fabs(c-b);
	    return b;			/* Acceptable approx. is found	*/
	}

	/* Decide if the interpolation can be tried	*/
	if( fabs(prev_step) >= tol_act	/* If prev_step was large enough*/
	    && fabs(fa) > fabs(fb) ) {	/* and was in true direction,
					 * Interpolation may be tried	*/
	    register double t1,cb,t2;
	    cb = c-b;
	    if( a==c ) {		/* If we have only two distinct	*/
					/* points linear interpolation	*/
		t1 = fb/fa;		/* can only be applied		*/
		p = cb*t1;
		q = 1.0 - t1;
	    }
	    else {			/* Quadric inverse interpolation*/

		q = fa/fc;  t1 = fb/fc;	 t2 = fb/fa;
		p = t2 * ( cb*q*(q-t1) - (b-a)*(t1-1.0) );
		q = (q-1.0) * (t1-1.0) * (t2-1.0);
	    }
	    if( p>(double)0 )		/* p was calculated with the */
		q = -q;			/* opposite sign; make p positive */
	    else			/* and assign possible minus to	*/
		p = -p;			/* q				*/

	    if( p < (0.75*cb*q-fabs(tol_act*q)/2) /* If b+p/q falls in [b,c]*/
		&& p < fabs(prev_step*q/2) )	/* and isn't too large	*/
		new_step = p/q;			/* it is accepted
						 * If p/q is too large then the
						 * bisection procedure can
						 * reduce [b,c] range to more
						 * extent */
	}

	if( fabs(new_step) < tol_act) {	/* Adjust the step to be not less*/
	    if( new_step > (double)0 )	/* than tolerance		*/
		new_step = tol_act;
	    else
		new_step = -tol_act;
	}
	a = b;	fa = fb;			/* Save the previous approx. */
	b += new_step;	fb = lamfunC(b,x,mu,wt,allw,L);	/* Do step to a new approxim. */
	if( (fb > 0 && fc > 0) || (fb < 0 && fc < 0) ) {
	    /* Adjust c for it to have a sign opposite to that of b */
	    c = a;  fc = fa;
	}

    }
    /* failed! */
    *Tol = fabs(c-b);
    *Maxit = -1;
    return b;
}



void cumsumsurv(double * x, double * s, int *LLL)
{
    double sum = 0.;
    int LL=LLL[0]-1;
    for (R_xlen_t i = 0 ; i < LLL[0] ; i++) {
	sum += x[LL-i];
	s[LL-i] = (double) sum;
    }
}


static inline double summm(double *x,int L){
int i=0;double re=0.;
for (;i<L;i++){
re+=x[i];
}
return(re);
}

static inline double maxabs(double *x, double mu,int L){
int i=0;double re=fabs(x[0]-mu);
for(;i<L;i++){
if (fabs(x[i]-mu)>re) re=fabs(x[i]-mu);
}
return(re);
}

void eltestwt(double *x, double *wt, double * mu1,int *Lx1,double *pi,double *lamre) {
double mu=mu1[0];
double Lx=Lx1[0];
double allw = summm(wt,Lx);
double BU = 0.02*allw/maxabs(x,mu,Lx);
double lam0=0.,lo,up;
double toldouble[1]={1e-9};
int MAXITER[1]={1000};
int i=0;
if (lamfunC(0,x,mu,wt,allw,Lx) == 0){ 
  lam0 = 0;
} else {
 if( lamfunC(0,x,mu,wt,allw,Lx) > 0 ) {
   lo = 0;
   up = BU;
    while(lamfunC(up,x,mu,wt,allw,Lx)>0){
         up += BU;
    }
  } else {
    up = 0;
    lo = - BU;
      while(lamfunC(lo,x,mu,wt,allw,Lx) < 0 ){
           lo  -= BU;
      }
     }
     /*
     double ax,				
    double bx,				
    double (*f)(double x),	
    double *Tol,		
    int *Maxit)
     */
     
 lam0 = R_zeroin2surv(lo,up,toldouble,MAXITER,x,mu,wt,allw,Lx);
}

for (i=0;i<Lx;i++){ pi[i] = wt[i]/(allw + lam0*(x[i]-mu));}
lamre[0]=lam0;

}


