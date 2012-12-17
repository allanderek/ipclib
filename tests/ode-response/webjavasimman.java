import java.util.*;
import java.io.*;

public class webjavasimman {

   private static double getPercent(int p, int t){
     return 100 * ((double) p) / ((double) t) ;
   } 
    
   private static double timeLimit = 199.5 ;
   public static double[] times = {0.5
          , 1.5
          , 2.5
          , 3.5
          , 4.5
          , 5.5
          , 6.5
          , 7.5
          , 8.5
          , 9.5
          , 10.5
          , 11.5
          , 12.5
          , 13.5
          , 14.5
          , 15.5
          , 16.5
          , 17.5
          , 18.5
          , 19.5
          , 20.5
          , 21.5
          , 22.5
          , 23.5
          , 24.5
          , 25.5
          , 26.5
          , 27.5
          , 28.5
          , 29.5
          , 30.5
          , 31.5
          , 32.5
          , 33.5
          , 34.5
          , 35.5
          , 36.5
          , 37.5
          , 38.5
          , 39.5
          , 40.5
          , 41.5
          , 42.5
          , 43.5
          , 44.5
          , 45.5
          , 46.5
          , 47.5
          , 48.5
          , 49.5
          , 50.5
          , 51.5
          , 52.5
          , 53.5
          , 54.5
          , 55.5
          , 56.5
          , 57.5
          , 58.5
          , 59.5
          , 60.5
          , 61.5
          , 62.5
          , 63.5
          , 64.5
          , 65.5
          , 66.5
          , 67.5
          , 68.5
          , 69.5
          , 70.5
          , 71.5
          , 72.5
          , 73.5
          , 74.5
          , 75.5
          , 76.5
          , 77.5
          , 78.5
          , 79.5
          , 80.5
          , 81.5
          , 82.5
          , 83.5
          , 84.5
          , 85.5
          , 86.5
          , 87.5
          , 88.5
          , 89.5
          , 90.5
          , 91.5
          , 92.5
          , 93.5
          , 94.5
          , 95.5
          , 96.5
          , 97.5
          , 98.5
          , 99.5
          , 100.5
          , 101.5
          , 102.5
          , 103.5
          , 104.5
          , 105.5
          , 106.5
          , 107.5
          , 108.5
          , 109.5
          , 110.5
          , 111.5
          , 112.5
          , 113.5
          , 114.5
          , 115.5
          , 116.5
          , 117.5
          , 118.5
          , 119.5
          , 120.5
          , 121.5
          , 122.5
          , 123.5
          , 124.5
          , 125.5
          , 126.5
          , 127.5
          , 128.5
          , 129.5
          , 130.5
          , 131.5
          , 132.5
          , 133.5
          , 134.5
          , 135.5
          , 136.5
          , 137.5
          , 138.5
          , 139.5
          , 140.5
          , 141.5
          , 142.5
          , 143.5
          , 144.5
          , 145.5
          , 146.5
          , 147.5
          , 148.5
          , 149.5
          , 150.5
          , 151.5
          , 152.5
          , 153.5
          , 154.5
          , 155.5
          , 156.5
          , 157.5
          , 158.5
          , 159.5
          , 160.5
          , 161.5
          , 162.5
          , 163.5
          , 164.5
          , 165.5
          , 166.5
          , 167.5
          , 168.5
          , 169.5
          , 170.5
          , 171.5
          , 172.5
          , 173.5
          , 174.5
          , 175.5
          , 176.5
          , 177.5
          , 178.5
          , 179.5
          , 180.5
          , 181.5
          , 182.5
          , 183.5
          , 184.5
          , 185.5
          , 186.5
          , 187.5
          , 188.5
          , 189.5
          , 190.5
          , 191.5
          , 192.5
          , 193.5
          , 194.5
          , 195.5
          , 196.5
          , 197.5
          , 198.5
          , 199.5};
   public static int numberBuckets = 200 ; 
   private static int samples   = 10000 ;
   public static void main(String[] args) {
     int buckets[]    = new int[ numberBuckets] ;
     double cdfBuckets[]    = new double[ numberBuckets] ;
     int outOfTime    = 0 ;
     Sampler sampler  = new Sampler (timeLimit) ;
     SAMPLER_LOOP:
     for (int i = 0; i < samples; i++){
        double sampleTime = sampler.sample () ;
        for (int k = 0 ; k < numberBuckets; k++){
           if (sampleTime < times[k]){
             buckets[k]++ ; continue SAMPLER_LOOP ;
            }
        }
       // If we get here we ran out of time
       outOfTime++;
     }
     // Now we set up the cdf buckets
     int allDone      = 0 ;
     for(int i = 0; i < numberBuckets; i++) {
        allDone += buckets[i] ;
        cdfBuckets[i] = getPercent(allDone, samples) ;
     }


     // Now we just want to print out the numbers in each bucket
     for (int i = 0; i < numberBuckets; i++) {
       System.out.println
              ("# buckets[" + i + "] chosen " +
               buckets[i] + " times, i.e. " + 
               getPercent(buckets[i], samples) +
               "% of the time, the cdf is: " + cdfBuckets[i] ) ;
     }
     System.out.println
              ("We ran out of time: " + outOfTime + " i.e. " +
                getPercent (outOfTime, samples) + "% of the time") ;
     if (sampler.deadlocked > 0) {
        System.out.println ("We deadlocked (not in a target state) " 
                             + sampler.deadlocked + " times" ) ;
     }

    
    try
    {
       FileOutputStream fout = new FileOutputStream ("webjavasimman-cdf.csv");
       PrintStream pout = new PrintStream(fout) ;
       for (int i = 0; i < numberBuckets; i++){
         pout.println (times[i] + ", " + cdfBuckets[i] / 100.0);
       }
       // Close our output stream
       fout.close();
    // Catches any error conditions
    }
    catch (IOException e)
    {
       System.err.println ("Unable to write to file");
       System.exit(-1);
    }
   return ;
   }

}

class Sampler {
   public Sampler (double limitTime) {
      timeLimit = limitTime ;
   }
 
   private Random generator = new Random();
   private double timeLimit = 10.0  ;


   // sample from the exponential distribution 
   private double expDelay(double mean) {
     return - mean * Math.log(generator.nextDouble());
   }

   private List<Transition> nextTransitions ; 
   public int deadlocked = 0 ;

   /* Note shouldn't be called if nextTransitions is null */
   private Transition chooseMove () {
      int length       = nextTransitions.size () ;    
      double totalRate = 0 ;
      for (int i = 0; i < length ; i++){
        double thisRate = (nextTransitions.get(i)).rate ;
        totalRate += thisRate ;
      }
      double passedProbability = 0 ;
      double picker            = generator.nextDouble () ;
      double chooser           = totalRate * picker ;

      for (int k = 0; k < length ; k++){
        Transition kTrans = nextTransitions.get(k) ;
        passedProbability += kTrans.rate ;
        if (chooser < passedProbability) {
          return (new Transition (kTrans.stateRepr, totalRate, kTrans.action)) ;
        }
      }
      System.out.println ("WE SHOULD NEVER GET HERE!!!") ;
      // If we haven't returned by now, it's the last one
      double rate       = totalRate ;
      Transition chosen = nextTransitions.get(length - 1) ;
      return (new Transition (chosen.stateRepr, rate, chosen.action)) ;
   }

   public double sample() {
     double total    = 0.0 ; 
     double dice     = 0.0 ;
     StateRepr state = new StateRepr () ;


     while (total <= timeLimit && !state.isTargetState()){
        nextTransitions       = state.getNext () ;
        /* We might be in a deadlocked state, if this occurs then
           nextTransitions will be null, we simply break out of the
           while loop since we will not be in the target state but
           never will be either.
        */
        if (nextTransitions.isEmpty())
           { total = timeLimit ; deadlocked++ ; break ; }
        Transition transition = chooseMove () ;
        state                 = transition.stateRepr ;
        total                 += expDelay(1 / transition.rate) ;
     }
     return total ;
   }
}// End of class Sampler

class StateRepr implements Cloneable {

  private int server = 0 ; // Number of processes in server
  private int browsing = 0 ; // Number of processes in browsing
  private int buying = 0 ; // Number of processes in buying
  private int user = 0 ; // Number of processes in user
  private int taggedBrowsing = 0 ; // Number of processes in taggedBrowsing
  private int taggedBuying = 0 ; // Number of processes in taggedBuying
  private int taggedDone = 0 ; // Number of processes in taggedDone
  private int taggedUser = 0 ; // Number of processes in taggedUser


  // The initial state is represented here.
  public StateRepr () { server = 10 ; 
                        user   = 300 ; 
                        browsing = 53 ; // ie. 54 - 1
                        buying   = 646;
                        taggedBrowsing = 1 ;
                      }

  public StateRepr clone (){
    StateRepr result = new StateRepr () ;
   result.server = server ;
   result.browsing = browsing ;
   result.buying = buying ;
   result.user = user ;
   result.taggedBrowsing = taggedBrowsing ;
   result.taggedBuying = taggedBuying ;
   result.taggedDone = taggedDone ;
   result.taggedUser = taggedUser ;

    return result ;
  }
 
  public boolean isTargetState () { return ((taggedDone == 1)) ;  }

  public double min(double a, double b) {
       return Math.min(a,b) ; }

  /* Now we are representing the model P1[4][a]
  */

  public LinkedList<Transition> getNext () {
    LinkedList <Transition> results = new LinkedList<Transition> () ;
    // These switch statements are in parallel because the
    // two components are composed with ||
    StateRepr next ;

if (user > 0){
 next = this.clone () ;
next.user-- ; 

next.browsing++ ; 

double localRate = (user * 0.9);
  if (localRate > 0.0) {     results.add (new Transition (next, localRate, "browse")) ; }}
if (user > 0){
 next = this.clone () ;
next.user-- ; 

next.buying++ ; 

double localRate = (user * 0.1);
  if (localRate > 0.0) {     results.add (new Transition (next, localRate, "buy")) ; }}
if (taggedUser > 0){
 next = this.clone () ;
next.taggedUser-- ; 

next.taggedBrowsing++ ; 

double localRate = (taggedUser * 0.9);
  if (localRate > 0.0) {     results.add (new Transition (next, localRate, "browse")) ; }}
if (taggedUser > 0){
 next = this.clone () ;
next.taggedUser-- ; 

next.taggedBuying++ ; 

double localRate = (taggedUser * 0.1);
  if (localRate > 0.0) {     results.add (new Transition (next, localRate, "buy")) ; }}
if (server > 0 && browsing > 0){
 next = this.clone () ;
next.server-- ; 
next.browsing-- ; 

next.server++ ; 
next.user++ ; 

double localRate = ((((server * 35.0) / (server * 35.0))
  *
  ((browsing * 5.0) / ((browsing * 5.0) + (taggedBrowsing * 5.0))))
 *
 (min
  ((server * 35.0) , ((browsing * 5.0) + (taggedBrowsing * 5.0)))));
  if (localRate > 0.0) {     results.add (new Transition (next, localRate, "getPage")) ; }}
if (server > 0 && taggedBrowsing > 0){
 next = this.clone () ;
next.server-- ; 
next.taggedBrowsing-- ; 

next.server++ ; 
next.taggedUser++ ; 

double localRate = ((((server * 35.0) / (server * 35.0))
  *
  ((taggedBrowsing * 5.0)
   /
   ((browsing * 5.0) + (taggedBrowsing * 5.0))))
 *
 (min
  ((server * 35.0) , ((browsing * 5.0) + (taggedBrowsing * 5.0)))));
  if (localRate > 0.0) {     results.add (new Transition (next, localRate, "getPage")) ; }}
if (server > 0 && buying > 0){
 next = this.clone () ;
next.server-- ; 
next.buying-- ; 

next.server++ ; 
next.user++ ; 

double localRate = ((((server * 3) / (server * 3))
  *
  ((buying * 5.0) / ((buying * 5.0) + (taggedBuying * 5.0))))
 *
 (min ((server * 3) , ((buying * 5.0) + (taggedBuying * 5.0)))));
  if (localRate > 0.0) {     results.add (new Transition (next, localRate, "getConfirm")) ; }}
if (server > 0 && taggedBuying > 0){
 next = this.clone () ;
next.server-- ; 
next.taggedBuying-- ; 

next.server++ ; 
next.taggedDone++ ; 

double localRate = ((((server * 3) / (server * 3))
  *
  ((taggedBuying * 5.0) / ((buying * 5.0) + (taggedBuying * 5.0))))
 *
 (min ((server * 3) , ((buying * 5.0) + (taggedBuying * 5.0)))));
  if (localRate > 0.0) {     results.add (new Transition (next, localRate, "getConfirm")) ; }}

    return results ;
  }
}

class Transition {
  public StateRepr stateRepr ;
  public double rate ;
  public String action ;

  public Transition (StateRepr s, double r, String a) {
    stateRepr = s ;
    rate      = r ;
    action    = a ;
  }
}
