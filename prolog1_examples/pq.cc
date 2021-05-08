/* (C) Miklos & Peter Szeredi, 2003 */

/* This C++ program is a rough translation of the following Prolog code

p(X, Y) :- 
        q(X, Z),         % this subgoal is referred to by qaptr
        p(Z, Y).         % this subgoal is referred to by pptr
p(X, Y) :- 
        q(X, Y).         % this subgoal is referred to by qbptr

q(1, 2).
q(2, 3).
q(2, 4).

main(X) :-
	(   p(X, Y), write('result = '), write(Y), nl, fail
	;   write('no (more) solutions'), nl
	).

The purpose of this excercise is to illustrate how the Procedure Box model
can be implemented. We are interested in showing how to map the Prolog flow
of control to C++. Therefore only one input-output pattern is supported:
both p/2 and q/2 should only be called with the first argument being input
and the second argument output.

The object created for p/2 holds pointers to the instances of the three
subgoals within p/2 (qaptr, pptr, qbptr), the current clause counter (int
clno) and the variables (int x, *py, z; py is a pointer, as it is used in
the output argument position).

The Call port is activated in two steps:

  - first, a new instance of the predicate in question is created. The
    constructor receives the predicate arguments and stores them in the
    instance. It also initialises the current clause counter to 0.

  - second, the `next' method of the object is called, to return the first
    solution,

The `next' method returns a Boolean, which is

  - TRUE for the Exit port, and
  - FALSE for the Fail port

The redo port is activated by simply calling the `next' method.

*/


#include <stdio.h>
#include <stdlib.h>

typedef enum {FALSE, TRUE} boolean;

class q;

class p {
  int clno;                  // current clause number
  q *qaptr;                  // points to the first subgoal q within p
  q *qbptr;                  // points to the second subgoal q within p
  p *pptr;                   // points to the recursive call of p within p
  int x;                     // holds the input argument
  int *py;                   // holds a pointer to the output argument
  int z;                     // holds the local variable
  

public:
  p(int _x, int *_py) : clno(0),x(_x),py(_py) {}
                             // the constructor stores the arguments
                             // and initialises the clause counter
  boolean next();
};

class q {
  int clno;
  int x;
  int *py;

public:
  q(int _x, int *_py)  : clno(0),x(_x),py(_py) {}
  boolean next();
};

boolean p::next()
{
  switch(clno) {
  case 0:                    // entry point for the Call port
    clno = 1;                // enter clause 1                 
								       
    qaptr = new q(x, &z);    // create a new instance of subgoal q(X,Z)
  redo11:							       
    if(!qaptr->next()) {     // if q(X,Z) fails                        
      delete qaptr;          // destroy it,			       
      goto cl2;              // and continue with clause 2 of p/2      
    }								       
                             // otherwise (q(X,Z) was successful)       
    pptr = new p(z, py);     // create a new instance of subgoal p(Z,Y) 
  case 1:                    // (enter here for Redo port if clno==1)
    /* redo12: */            
    if(!pptr->next()) {      // if p(Z,Y) fails
      delete pptr;           // destroy it,
      goto redo11;           // and continue at redo port of q(X,Z)
    }
                             // otherwise (p(Z,Y) was successful)       
    return TRUE;             // exit via the Exit port
    
  cl2:
    clno = 2;                // enter clause 2
    qbptr = new q(x, py);    // create a new instance of subgoal q(X,Y)
  case 2:                    // (enter here for Redo port if clno==1)                    
    /* redo21: */
    if(!qbptr->next()) {     // if q(X,Y) fails                        
      delete qbptr;	     // destroy it,                            
      return FALSE;	     // and exit via the Fail port
    }
                             // otherwise (q(X,Y) was successful)       
    return TRUE;             // exit via the Exit port

  default:                   // This point is never reached
    return FALSE;            
  }
}

// Note that, as an optimisation, we could have used the same variable for
// qaptr and qbptr in the p::next function above. This is because the
// objects pointed to by them never exist at the same time (the
// corresponding subgoals are in different clauses).  However, if the two
// q/2 subgoals occur in a conjunction, then they require separate
// pointers.

boolean q::next()
{
  switch(clno) {
  case 0:                    // entry point for the Call port
    clno = 1;                // enter clause 1

    if(x != 1)               // if fact q(1,...) does not unify with the call
      goto cl2;              // goto clause 2
    
                             // otherwise (q(1,2) unifies)
    *py = 2;                 // perform the output part of the unification
    return TRUE;             // exit via the Exit port

  case 1:                    // (enter here for Redo port if clno==1)
    /* redo10: */

  cl2:       
    clno = 2;                // enter clause 2
    if(x != 2)               // if fact q(2,...) does not unify with the call
      goto cl3;              // goto clause 3

                             // otherwise (q(2,3) unifies)    
    *py = 3;                 // perform the output part of the unification
    return TRUE;	     // exit via the Exit port                    
    
  case 2:                    // (enter here for Redo port if clno==2)
    /* redo20: */

  cl3:       
    clno = 3;                // enter clause 3
    if(x != 2)               // if fact q(2,...) does not unify with the call
      return FALSE;          // exit via the Fail port

                             // otherwise (q(2,4) unifies)    
    *py = 4;                 // perform the output part of the unification
    return TRUE;	     // exit via the Exit port                    
    
  case 3:                    // (enter here for Redo port if clno==3)
    /* redo30: */

  default:
    return FALSE;            // exit via the Fail port
  }
}


int main(int argc, char *argv[])
{
  if(argc != 2) {
    fprintf(stderr, "usage: %s input(int)\n", argv[0]);
    exit(1);
  }
  
  int x = atoi(argv[1]);
  int y;
  p *pptr;

  pptr = new p(x, &y);       // create a new instance of subgoal p(X,Y) 
  
 redo11:
  if(!pptr->next()) {        // if p(X,Y) fails 
    delete pptr;             // then destroy it
    printf("no (more) solutions\n");
                             // call BIPs: write('no (more) solutions'), nl
    exit(0);                 // exit main/2
  }
                             // otherwise (p(X,Y) was successful)         
  printf("result = %i\n", y);
                             // call BIPs: write('result = '), write(Y), nl
  goto redo11;               // execute BIP fail:
                             // continue at redo port of p(X,Y)
}
