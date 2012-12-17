// HYDRA version 0.9 (Linux) (18 September 2003)
// (C) 2003 Nicholas J Dingle & William J Knottenbelt, Imperial College London

// Automatically generated from 'tests/basic/good/test01.mod' on Thu Oct 29 16:13:14 2009

using namespace std;

#include <iostream>
#include <memory>
#include <cstdio>
#include <cmath>
#include <complex>
#include <cstdlib>
#include <cstring>
#include "dyna.hpp"
#include "common.hpp"
#include "element.hpp"
#include "user.hpp"
inline double ipc_min (double e, double l){ double a = e, b = l; return (a < b ? a : b); }
inline double ipc_apparent (double r1, double r2, double raP, double raQ)
{ return (r1/raP) * (r2/raQ) * (ipc_min(raP, raQ)) ; }
inline double ipc_active_passive (double r1, double r2, double raP, double raQ)
{ return (r1/raP) * (r2/raQ) * raP ; }
/* it's good to be back */

class State {
private:
  // state vector description
  short P1_2, P2_2, P1, P2, yyMasterProbeRunning, yyMasterProbeStopped, yyPassageProbeRunning, yyPassageProbeStopped;
  // ### do not change the relative offset of kids
  unsigned char __kids;

  static int __trans;
  static int *__enabled, *__priority;
  static TransType *__transitionType;

  unsigned long __primaryKey, __secondaryKey;
  StateType __type;

  int __numberEnabled;
  FastDynaArray<int> __enabledTrans;

  FastDynaArray<StateMatrixElement> __children;

public:
  State() {
  __kids = 0;
#ifdef STATE_COUNT
  __instances++;
#endif
#ifdef SAFE
  __primaryKey = __secondaryKey = -1;
#endif
  } //;
  ~State() {
#ifdef STATE_COUNT
  __destroyed++;
#endif
  } //;
  void initial();
  int isEnabled(int *__en) const;
  int isTarget() const;
  int isExcluded() const;
  double passageWeight() const;
  void fire(int __t, State *next) const;
  void calcHelpValues();
  void computeStateMeasures(double *) const;
  void computeCountMeasures(int *, int, double *) const;
  double getTransWeight(int __t) const;
  void checkInvariants() const;
  int input(FILE *);
  int output(FILE *) const;
  void output() const;
  unsigned long calcPrimaryHashKey() const;
  unsigned long calcSecondaryHashKey() const;
  long size() const;
  void calcHashKeys(unsigned long &__primary, unsigned long &__secondary);
  inline void computeHashKeys() {    calcHashKeys(__primaryKey, __secondaryKey); } //;
  inline StateType getType() const { return __type; } //;
  inline unsigned long getPrimaryKey() const { return __primaryKey; } //;
  inline unsigned long getSecondaryKey() const { return __secondaryKey; } //;
  int findEnabledTransitions();
  inline int getEnabledTransition(int __t) { return __enabledTrans(__t); } //;
  inline void fireEnabledTransition(int __t, State *next) const { fire(__enabledTrans(__t), next); } //;
  inline int numberChildren() const { return __kids; } //;
  inline int getNumberEnabled() const { return __numberEnabled; } //;
  inline void setKids(int __k) { __kids = __k; } //;
  inline double getEnabledTransWeight(int __t) const { return getTransWeight(__enabledTrans(__t)); } //;
  inline StateMatrixElement &getChild(int __t) const { return __children(__t); } //;
  inline double rateOut() { double __sum = 0; for (int __n=0; __n<__kids; __n++) __sum += __children(__n).getWeight(); return __sum; } //;
  inline double getTotalTransWeight() const {
    double __sum = 0;
    for (int __t=0; __t<getNumberEnabled(); __t++)
      __sum += getEnabledTransWeight(__t);
    return __sum;
  } //;
  inline void setChild(int __t, long __value, double __weight) {
    if (__weight <= 0) {
      output();
      cout << endl;
      return;
    }
    for (long __n=0; __n<__kids; __n++) {
      if (__children(__n).getDestination()==__value) {
        __children[__n].setWeight(__children(__n).getWeight()+__weight);
        return;
      }
    }
    __children.check(__kids);
    __children[__kids].setDestination(__value);
    __children[__kids].setWeight(__weight);
    if (__kids == 255 ) { printf ("Sorry too many kids\n "); exit(1) ; } ; __kids++;
  } //;
  inline int matrixOutput(FILE *__mf) {
    int __k = __kids;
    /*printf("hello again __k = %d\n", __k); */
     fwrite(&__k, sizeof(int), 1, __mf);
    return fwrite(__children.getArray(), sizeof(StateMatrixElement), __kids, __mf);
  } //;
  inline void calculateCountMeasures(double *__measures) const {
    computeCountMeasures(__enabledTrans.getArray(),
      __numberEnabled, __measures);
  } //;
  int initialise(int, TransType *,int *);
};


TransType *State::__transitionType = NULL;
int State::__trans = 0;
int *State::__enabled = NULL;
int *State::__priority = NULL;

void State::calcHashKeys(unsigned long &__primary, unsigned long &__secondary) {
  __primary = calcPrimaryHashKey();
  __secondary = calcSecondaryHashKey();
}

int State::initialise(int __transitions, TransType *__transType, int *__transPriority) {
  __trans = __transitions;
  __transitionType = new TransType[__trans];
  __enabled = new int[__trans];
  __priority = new int[__trans];
  for (int __n=0; __n<__trans; __n++) {
    __transitionType[__n] = __transType[__n];
    __priority[__n] = __transPriority[__n];
  }
  return 1;
}

int State::findEnabledTransitions() {
  int __vanishing = 0;
  long ___timedPriority = 0, __instantPriority = 0;
  __numberEnabled = 0;
  __type = TANGIBLE;
  isEnabled(__enabled);
  for (int __t=0; __t<__trans; __t++) {
    switch(__transitionType[__t]) {
    case TIMED:
      if (!__vanishing && __enabled[__t] && __priority[__t] >= ___timedPriority) {
        if (__priority[__t] > ___timedPriority) {
          __numberEnabled = 0;
          ___timedPriority = __priority[__t];
	   }
        __enabledTrans.check(__numberEnabled);
        __enabledTrans[__numberEnabled] = __t;
        __numberEnabled++;
      }
      break;
    case INSTANTANEOUS:
      if (__enabled[__t] && __priority[__t] >= __instantPriority) {
        if (!__vanishing) {
          __type = VANISHING;
          __numberEnabled = 0;
          __vanishing = 1;
         __instantPriority = __priority[__t];
        } else if (__priority[__t] > __instantPriority) {
	     __numberEnabled = 0;
	     __instantPriority = __priority[__t];
        }
        __enabledTrans.check(__numberEnabled);
        __enabledTrans[__numberEnabled] = __t;
        __numberEnabled++;
        break;
      }
    }
  }
  __enabledTrans.setLength(__numberEnabled);
  return __numberEnabled;
}

void State::initial() { // initial state vector
  P1_2 = 1 ;
  P2_2 = 0 ;
  P1 = 1 ;
  P2 = 0 ;
  yyMasterProbeRunning = 0 ;
  yyMasterProbeStopped = 1 ;
  yyPassageProbeRunning = 0 ;
  yyPassageProbeStopped = 1 ;
}

int State::isEnabled(int *__enabled) const {
  __enabled[   0] = ((P1_2 == 1) && (P1 == 1) && (yyMasterProbeStopped == 1) && (yyPassageProbeStopped == 1)); 
  __enabled[   1] = ((P1_2 == 1) && (P1 == 1) && (yyMasterProbeStopped == 1) && (yyPassageProbeStopped == 1)); 
  __enabled[   2] = ((P2_2 == 1) && (P1 == 1) && (yyMasterProbeRunning == 1) && (yyPassageProbeRunning == 1)); 
  __enabled[   3] = ((P2_2 == 1) && (P1 == 1) && (yyMasterProbeRunning == 1) && (yyPassageProbeRunning == 1)); 
  __enabled[   4] = ((P2_2 == 1) && (P1 == 1) && (yyMasterProbeStopped == 1) && (yyPassageProbeStopped == 1)); 
  __enabled[   5] = ((P2_2 == 1) && (P1 == 1) && (yyMasterProbeStopped == 1) && (yyPassageProbeStopped == 1)); 
  __enabled[   6] = ((P1_2 == 1) && (P2 == 1) && (yyMasterProbeRunning == 1) && (yyPassageProbeRunning == 1)); 
  __enabled[   7] = ((P1_2 == 1) && (P2 == 1) && (yyMasterProbeRunning == 1) && (yyPassageProbeRunning == 1)); 
  __enabled[   8] = ((P1_2 == 1) && (P2 == 1) && (yyMasterProbeStopped == 1) && (yyPassageProbeStopped == 1)); 
  __enabled[   9] = ((P1_2 == 1) && (P2 == 1) && (yyMasterProbeStopped == 1) && (yyPassageProbeStopped == 1)); 
  __enabled[  10] = ((P2_2 == 1) && (P2 == 1) && (yyMasterProbeRunning == 1) && (yyPassageProbeRunning == 1)); 
  __enabled[  11] = ((P2_2 == 1) && (P2 == 1) && (yyMasterProbeRunning == 1) && (yyPassageProbeRunning == 1)); 
  __enabled[  12] = ((P2_2 == 1) && (P2 == 1) && (yyMasterProbeRunning == 1) && (yyPassageProbeStopped == 1)); 
  __enabled[  13] = ((P2_2 == 1) && (P2 == 1) && (yyMasterProbeRunning == 1) && (yyPassageProbeStopped == 1)); 
  return 1;
}

int State::isTarget() const {
  return (yyMasterProbeStopped);
}

int State::isExcluded() const {
  return 0;
}

double State::passageWeight() const {
  return (  yyPassageProbeRunning);
}

void State::calcHelpValues() {
}

void State::checkInvariants() const {
}

void State::fire(int __t, State *next) const {
  memcpy(next, this, size());
  switch(__t) { // transition actions
  case    0:
    next -> P1 = 0 ;
    next -> yyMasterProbeStopped = 0 ;
    next -> yyPassageProbeStopped = 0 ;
    next -> P1_2 = 1 ;
    next -> P2 = 1 ;
    next -> yyMasterProbeRunning = 1 ;
    next -> yyPassageProbeRunning = 1 ;
    break;
  case    1:
    next -> P1_2 = 0 ;
    next -> yyMasterProbeStopped = 0 ;
    next -> yyPassageProbeStopped = 0 ;
    next -> P2_2 = 1 ;
    next -> P1 = 1 ;
    next -> yyMasterProbeRunning = 1 ;
    next -> yyPassageProbeRunning = 1 ;
    break;
  case    2:
    next -> P1 = 0 ;
    next -> yyPassageProbeRunning = 0 ;
    next -> P2_2 = 1 ;
    next -> P2 = 1 ;
    next -> yyMasterProbeRunning = 1 ;
    next -> yyPassageProbeStopped = 1 ;
    break;
  case    3:
    next -> P2_2 = 0 ;
    next -> yyMasterProbeRunning = 0 ;
    next -> yyPassageProbeRunning = 0 ;
    next -> P1_2 = 1 ;
    next -> P1 = 1 ;
    next -> yyMasterProbeStopped = 1 ;
    next -> yyPassageProbeStopped = 1 ;
    break;
  case    4:
    next -> P1 = 0 ;
    next -> yyMasterProbeStopped = 0 ;
    next -> yyPassageProbeStopped = 0 ;
    next -> P2_2 = 1 ;
    next -> P2 = 1 ;
    next -> yyMasterProbeRunning = 1 ;
    next -> yyPassageProbeRunning = 1 ;
    break;
  case    5:
    next -> P2_2 = 0 ;
    next -> P1_2 = 1 ;
    next -> P1 = 1 ;
    next -> yyMasterProbeStopped = 1 ;
    next -> yyPassageProbeStopped = 1 ;
    break;
  case    6:
    next -> P1_2 = 0 ;
    next -> yyPassageProbeRunning = 0 ;
    next -> P2_2 = 1 ;
    next -> P2 = 1 ;
    next -> yyMasterProbeRunning = 1 ;
    next -> yyPassageProbeStopped = 1 ;
    break;
  case    7:
    next -> P2 = 0 ;
    next -> yyMasterProbeRunning = 0 ;
    next -> yyPassageProbeRunning = 0 ;
    next -> P1_2 = 1 ;
    next -> P1 = 1 ;
    next -> yyMasterProbeStopped = 1 ;
    next -> yyPassageProbeStopped = 1 ;
    break;
  case    8:
    next -> P1_2 = 0 ;
    next -> yyMasterProbeStopped = 0 ;
    next -> yyPassageProbeStopped = 0 ;
    next -> P2_2 = 1 ;
    next -> P2 = 1 ;
    next -> yyMasterProbeRunning = 1 ;
    next -> yyPassageProbeRunning = 1 ;
    break;
  case    9:
    next -> P2 = 0 ;
    next -> P1_2 = 1 ;
    next -> P1 = 1 ;
    next -> yyMasterProbeStopped = 1 ;
    next -> yyPassageProbeStopped = 1 ;
    break;
  case   10:
    next -> P2 = 0 ;
    next -> yyMasterProbeRunning = 0 ;
    next -> yyPassageProbeRunning = 0 ;
    next -> P2_2 = 1 ;
    next -> P1 = 1 ;
    next -> yyMasterProbeStopped = 1 ;
    next -> yyPassageProbeStopped = 1 ;
    break;
  case   11:
    next -> P2_2 = 0 ;
    next -> yyMasterProbeRunning = 0 ;
    next -> yyPassageProbeRunning = 0 ;
    next -> P1_2 = 1 ;
    next -> P2 = 1 ;
    next -> yyMasterProbeStopped = 1 ;
    next -> yyPassageProbeStopped = 1 ;
    break;
  case   12:
    next -> P2 = 0 ;
    next -> yyMasterProbeRunning = 0 ;
    next -> P2_2 = 1 ;
    next -> P1 = 1 ;
    next -> yyMasterProbeStopped = 1 ;
    next -> yyPassageProbeStopped = 1 ;
    break;
  case   13:
    next -> P2_2 = 0 ;
    next -> yyMasterProbeRunning = 0 ;
    next -> P1_2 = 1 ;
    next -> P2 = 1 ;
    next -> yyMasterProbeStopped = 1 ;
    next -> yyPassageProbeStopped = 1 ;
    break;
  default  :
    cout << "fire() error: transition " << __t << " out of range!" << endl;
  }
}

double State::getTransWeight(int __t) const
{
  switch (__t) {
  case    0: return 1;
  case    1: return 1;
  case    2: return 1;
  case    3: return 1;
  case    4: return 1;
  case    5: return 1;
  case    6: return 1;
  case    7: return 1;
  case    8: return 1;
  case    9: return 1;
  case   10: return 1;
  case   11: return 1;
  case   12: return 1;
  case   13: return 1;
  default  :
    cout << "getTransWeight() error: transition " << __t << " out of range!" << endl;
  }
  return 0.0;
}

void State::computeStateMeasures(double *__measures) const
{
}

void State::computeCountMeasures(int *__enabledTrans, int __numberEnabled, double *__measures) const
{
  int __trans, __t;
  static State *__next = new State;
  for (__trans = 0; __trans < __numberEnabled; __trans++) {
    __t = __enabledTrans[__trans];
  }
  // delete next;
}

unsigned long State::calcPrimaryHashKey() const
{
  register unsigned long __key = P1_2 ^ (P2_2 << 2) ^ (P1 << 4) ^ (P2 << 6) ^ (yyMasterProbeRunning << 8) ^ (yyMasterProbeStopped << 10) ^ (yyPassageProbeRunning << 12) ^ (yyPassageProbeStopped << 14);
  return (__key % 350003);
}

unsigned long State::calcSecondaryHashKey() const
{
  register unsigned long __key1=0, __key2=0, __sum=0, __s1=0, __s2=16;
  __key1 ^= (P1_2 << __s1);
  __sum += P1_2;
  __key2 ^= (__sum << __s2);
  __s1 += 3; __s1 %= 29;
  __s2 += 5; __s2 %= 29;
  __key1 ^= (P2_2 << __s1);
  __sum += P2_2;
  __key2 ^= (__sum << __s2);
  __s1 += 3; __s1 %= 29;
  __s2 += 5; __s2 %= 29;
  __key1 ^= (P1 << __s1);
  __sum += P1;
  __key2 ^= (__sum << __s2);
  __s1 += 3; __s1 %= 29;
  __s2 += 5; __s2 %= 29;
  __key1 ^= (P2 << __s1);
  __sum += P2;
  __key2 ^= (__sum << __s2);
  __s1 += 3; __s1 %= 29;
  __s2 += 5; __s2 %= 29;
  __key1 ^= (yyMasterProbeRunning << __s1);
  __sum += yyMasterProbeRunning;
  __key2 ^= (__sum << __s2);
  __s1 += 3; __s1 %= 29;
  __s2 += 5; __s2 %= 29;
  __key1 ^= (yyMasterProbeStopped << __s1);
  __sum += yyMasterProbeStopped;
  __key2 ^= (__sum << __s2);
  __s1 += 3; __s1 %= 29;
  __s2 += 5; __s2 %= 29;
  __key1 ^= (yyPassageProbeRunning << __s1);
  __sum += yyPassageProbeRunning;
  __key2 ^= (__sum << __s2);
  __s1 += 3; __s1 %= 29;
  __s2 += 5; __s2 %= 29;
  __key1 ^= (yyPassageProbeStopped << __s1);
  __sum += yyPassageProbeStopped;
  __key2 ^= (__sum << __s2);
  return (__key1 ^ __key2);
}

long State::size() const {
    return (long) ( (unsigned char *) &__kids - (unsigned char *) this );
}

void State::output() const
{
  cout << "(" << P1_2 << "," << P2_2 << "," << P1 << "," << P2 << "," << yyMasterProbeRunning << "," << yyMasterProbeStopped << "," << yyPassageProbeRunning << "," << yyPassageProbeStopped << ")" << endl;
}

int State::input(FILE *__fp) {
  return fread(this, size(), 1, __fp);
}

int State::output(FILE *__fp) const {
  return fwrite(this, size(), 1, __fp);
}

Access::Access() {
  __current = new State;
}

Access::~Access() {
  delete __current;
}

void Access::initial() {
  __current->initial();
}

int Access::isEnabled(int *__enabled) const {
  return __current->isEnabled(__enabled);
}

int Access::isTarget() const {
  return __current->isTarget();
}

int Access::isExcluded() const {
  return __current->isExcluded();
}

double Access::passageWeight() const {
  return __current->passageWeight();
}

void Access::fire(int __t, Access *next) const {
  __current->fire(__t, next->__current);
}

void Access::computeStateMeasures(double *__measures) const {
  __current->computeStateMeasures(__measures);
}

void Access::computeCountMeasures(int *__enabled, int __numberEnabled, double *__measures) const {
  __current->computeCountMeasures(__enabled, __numberEnabled, __measures);
}

double Access::getTransWeight(int __t) const {
  return __current->getTransWeight(__t);
}

int Access::input(FILE *__fp) {
  return __current->input(__fp);
}

int Access::output(FILE *__fp) {
  return __current->output(__fp);
}

void Access::output() {
  __current->output();
}

long Access::size() {
  return __current->size();
}

void Access::computeHashKeys() { return __current->computeHashKeys(); }

unsigned long Access::getPrimaryKey() { return __current->getPrimaryKey(); }

unsigned long Access::getSecondaryKey() { return __current->getSecondaryKey(); }

StateType Access::getType() { return __current->getType(); }
int Access::findEnabledTransitions() { return __current->findEnabledTransitions(); }

int Access::getEnabledTransition(int __t) { return __current->getEnabledTransition(__t); }

void Access::fireEnabledTransition(int __t, Access *next) {
  return __current->fireEnabledTransition(__t, next->__current);
}

int Access::numberChildren() { return __current->numberChildren(); }

int Access::getNumberEnabled() { return __current->getNumberEnabled(); }

void Access::setKids(int __k) { return __current->setKids(__k); }

double Access::getEnabledTransWeight(int __t) { return __current->getEnabledTransWeight(__t); }

StateMatrixElement& Access::getChild(int __t) { return __current->getChild(__t); }

double Access::rateOut() { return __current->rateOut(); }

double Access::getTotalTransWeight() { return __current->getTotalTransWeight(); }

void Access::setChild(int __t, long __value, double __weight) { return __current->setChild(__t,__value,__weight); }

int Access::matrixOutput(FILE *__mf) { return __current->matrixOutput(__mf); }

void Access::calculateCountMeasures(double *__measures) { return __current->calculateCountMeasures(__measures); }

int Access::initialise(int __a, TransType *__b, int *__c) { return __current->initialise(__a,__b,__c); }

