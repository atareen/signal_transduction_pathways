/**
 * Program that computes mutual information of single input single output channel
 * Revised 08/28/2018
 */
import java.util.Random;

public class MI_1D 
{
	
	public static void main(String[] args)
	{
		
		/*
		 *  Setup up global variables and instantiate input probability distribution in an array
		 */
		
		double numberBins = 100;
		double V;				// Volume parameter
		double alpha = 1.0;		// background activation/deactivation parameter
		
		// step size
		double dx = 1/numberBins;
		
		double stDev1 = 0.01;	//	standard deviation is 0.01*0.01 = 0.1, mean is 0.5
		double mu1 = 0.5;
		
		double input1Distribution[] = new double[1000000+1];
		double Inp1 = 0;
		 
		// seeding this random number gives the same Gaussian distribution every time
		Random random = new Random(1);
			
		// loop that generates random Gaussian numbers, which are later histogrammed to give the input Gaussian distribution
		for(int index=0;index<input1Distribution.length;index++)
		{
			Inp1 = Math.sqrt(stDev1)*random.nextGaussian()+mu1;
		
			input1Distribution[index]  = Math.abs(Inp1);
		}
		
		// double dx = (max-min)/numberBins;
		double min = 0;
		double max = 1;
		
		// changing input Series into histograms
		double input1Hist[] = calcHistogram(input1Distribution, (int)min, (int)max,(int)numberBins);
		
		// the length of these arrays = binSize chosen above
		double p_x[] = new double[(int)numberBins];
		
		double input1Hist_Norm = 0;
		
		// compute normalization constant for input distribution
		for(int i=0;i<input1Hist.length;i++)
		{
				input1Hist_Norm+=dx*input1Hist[i];
		}

		// prob_I1 length is equal to numberBins, prob_I1 represents probability density
		// normalize probability
		for(int i=0;i<input1Hist.length;i++)
		{
				p_x[i]=input1Hist[i]/input1Hist_Norm;
		}
		
		// input and output arrays
		double[] I1 = new double[(int)numberBins];
		double[] y1 = new double[(int)numberBins];
				
		for(int i=0;i<I1.length;i++)
		{
			if(i==0)
			{
				I1[i] = dx/2.0;			
				y1[i] = dx/2.0;
			}
			else
			{
				I1[i] = dx + I1[i-1];
				y1[i] = dx + I1[i-1];
			}
			//System.out.println(I1[i]);
		}
		
		/*
		 * Loop that calculates mutual information.
		 * Contains nested loops that compute conditional, marginal, and joint probabilites required to compute MI.
		 */
		
		// loop over volumes if needed, currently only V = 20 will be used.
		for(V=50;V<51;V+=10)
		{
			double maxMI = 0;						// optional variable to record maximum mutual information
			int nanCounter = 0;						// debug variable, counts the number of nan's produced
			for(double k=0.1;k<50;k+=0.1)
			{
				// arrays for conditional probability, joint probability and marginal output probability.
				double p_y_given_x[][] = new double[p_x.length][p_x.length];
				double p_y_x[][] = new double[p_x.length][p_x.length];
				double p_y[] = new double[p_x.length];
				
				// array that will hold normalization constants for the conditional probability.
				double conditionalNorm[] = new double[(int)numberBins];
				
				// the following two loops sweep over outputs and inputs to compute probability of output given input, i.e. p_y_given_x[i][j]
				// sweep over outputs
				for(int i=0;i<p_y.length;i++)
				{
					// sweep over inputs
					for(int j=0;j<p_x.length;j++)
					{
						// compute different parts of the conditional probability
						double e =  (2*V*y1[i]* (I1[j]*I1[j]*k*k-alpha*alpha))/((alpha-I1[j]*k)*(alpha-I1[j]*k)) ;
						double a,b;
						a = -Math.log(1+((alpha-I1[j]*k)*y1[i])/(I1[j]*k)) ;
						b = (4*I1[j]*k*V*alpha/((alpha-I1[j]*k)*(alpha-I1[j]*k)))*Math.log(1+((alpha-I1[j]*k)*y1[i])/(I1[j]*k));
												
						p_y_given_x[i][j] = Math.exp(e+a+b);
												
						// debug conditional statement if there happens to be a computational overflow or error. Will currently stop the program on error.
						if(Double.isNaN(p_y_given_x[i][j]) == true)
						{
							System.out.println("i : "+i+", j: "+j);
							System.out.println("Output: "+y1[i]+" Input: "+I1[j]);
							System.out.print("First Term: ");
							System.out.println(Math.exp(-2*V*y1[i]*(I1[j]*k+alpha)/(alpha-I1[j]*k)));
							System.out.print("First Term exponent: ");
							System.out.println(-2*V*y1[i]*(I1[j]*k+alpha)/(alpha-I1[j]*k));
							System.out.print("Second Term: ");						
							System.out.println(Math.pow(1+((alpha-I1[j]*k)*y1[i])/(I1[j]*k),(4*I1[j]*k*alpha*V)/((alpha-I1[j]*k)*(alpha-I1[j]*k))-1));
							System.out.print("Second term base: ");
							System.out.println(1+((alpha-I1[j]*k)*y1[i])/(I1[j]*k));
							System.out.print("Second term exponent: ");
							System.out.println((4*I1[j]*k*alpha*V)/((alpha-I1[j]*k)*(alpha-I1[j]*k))-1);
							System.out.print("alpha-I*k: ");
							System.out.println(Math.abs(alpha-I1[j]*k));
							nanCounter++;
							System.out.println("nanCounter: "+nanCounter);
							System.out.println();
							System.exit(0);
						}
						
						// snippet to check un-normalized conditional probability with mathematica at output = input = 0.515
						/*
						if(Math.abs(y1[i]-0.515)<1e-6 & Math.abs(I1[j]-0.515)<1e-6)
						{
							System.out.println(I1[j]+" "+y1[i]+" "+p_y_given_x[i][j]);
							//System.out.println(I1[j]+" "+i+" "+p_y_given_x[i][j]);
							System.exit(0);
						}
						*/
						
					}	// for j
				}	// end conditional probability loop
		
				// 2nd conditional probability loop for gathering normalization constants
				for(int i=0;i<p_y.length;i++)
				{
					for(int j=0;j<p_x.length;j++)
					{
						// for a given input, sweep over outputs and store the value in a variable
						conditionalNorm[i]+=p_y_given_x[j][i];
						
					}	// for j
					//System.out.println(conditionalNorm[i]);
					
				}	// end conditional probability loop
				//System.exit(0);
				
				// 3rd loop for normalizing conditional probability
				for(int i=0;i<p_y.length;i++)
				{
					// get conditional output probability
					for(int j=0;j<p_x.length;j++)
					{
						p_y_given_x[j][i]/=(conditionalNorm[i]*dx);	// area under curve for probability sums to 1 for each given input 
					}	
				}	// end conditional probability loop
								
				// p_y loop: to get p_y from p_y|x: p(y=1) = sum(i=1,4) p(y=1|x=i)*p(x=i)
				// and printing loop
				for(int i=0;i<p_y.length;i++)
				{
					for(int j=0;j<p_x.length;j++)
					{
						p_y[i]+=p_y_given_x[i][j]*p_x[j]*dx;
						//System.out.print(p_y_given_x[i][j]+" ");
					}	// for j
					//System.out.println(p_y[i]);
				}	// end conditional probability loop
								
				// compute joint probability
				for(int i=0;i<p_y.length;i++)
				{
					for(int j=0;j<p_x.length;j++)
					{
						p_y_x[i][j]=p_y_given_x[i][j]*p_x[j];
						//System.out.println(I1[j]+" "+y1[i]+" "+p_y_x[i][j]+" ");
					}	// for j
				}
				
				// Compute mutual information
				double MI = 0;
				for(int i=0;i<p_y.length;i++)
				{
					for(int j=0;j<p_x.length;j++)
					{
						if(p_y_x[i][j]>0)
						{
							//MI+=dx*dx*p_y_x[i][j]*Math.log(p_y_x[i][j]/(p_x[j]*p_y[i]));
							MI+=p_y_x[i][j]*Math.log(p_y_x[i][j]/(p_x[j]*p_y[i]))*dx*dx;
						}					
					}
				}
				
				if(MI>maxMI)
				{
					maxMI=MI;
				}
				System.out.println(k+" "+MI);
				//System.out.println(MI);
							
			} // for k
			//System.out.println(V+" "+maxMI);
		}	// for V
	}	// main
	
	
	public static double[] calcHistogram(double[] data, double min, double max, int numBins) 
	{
		  final double[] result = new double[numBins];
		  final double binSize = (max - min)/numBins;

		  for (double d : data) {
		    int bin = (int) ((d - min) / binSize);
		    if (bin < 0) { /* this data is smaller than min */ }
		    else if (bin >= numBins) { /* this data point is bigger than max */ }
		    else {
		      result[bin] += 1;
		    }
		  }
		  return result;
	}
	
}
