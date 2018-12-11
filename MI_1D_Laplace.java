import java.util.Random;


public class MI_1D_Laplace 
{
	
	public static void main(String[] args)
	{
		
		double numberBins = 100;
		double V;
		
		// step size
		double dx = 1/numberBins;
		
		double stDev1 = 0.01;
		double mu1 = 0.5;
		
		double input1Distribution[] = new double[1000000+1];
		//double P_O1_O2[][];
		
		double Inp1 = 0;
		 
		// seeding this random number gives the same gaussian distribution every time
		Random random = new Random(1);
			
		for(int index=0;index<input1Distribution.length;index++)
		{
			Inp1 = Math.sqrt(stDev1)*random.nextGaussian()+mu1;
		
			input1Distribution[index]  = Math.abs(Inp1);
		}
		
		double min = 0;
		double max = 1;
		
		// changing input Series into histograms
		double input1Hist[] = calcHistogram(input1Distribution, (int)min, (int)max,(int)numberBins);
		
		// the length of these arrays = binSize chosen above
		double p_x[] = new double[(int)numberBins];
		
		double input1Hist_Norm = 0;
		
		for(int i=0;i<input1Hist.length;i++)
		{
				input1Hist_Norm+=dx*input1Hist[i];
		}

		//double dx = (max-min)/numberBins;
		// prob_I1 length is equal to numberBins, prob_I1 represents probability density
		double iIndex = 0.005;
		for(int i=0;i<input1Hist.length;i++)
		{
				p_x[i]=input1Hist[i]/input1Hist_Norm;
				//System.out.println(iIndex+" "+p_x[i]);
				iIndex+=0.01;
		}
		//System.exit(0);
		
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
		
		double Otot = 1;
		double C = 1;
		for(V=50;V<51;V+=10)
		{
			double maxMI = 0;
			//for(double k=0.01;k<50;k+=0.1)
			for(double k=0.1;k<50;k+=0.1)
			{
				double alpha = 1.0;
				double conditionalNorm[] = new double[(int)numberBins];
				double p_y_given_x[][] = new double[p_x.length][p_x.length];
				double p_y_x[][] = new double[p_x.length][p_x.length];
				double p_y[] = new double[p_x.length];
			
				double p_y_x_Norm = 0;
				double p_y_Norm = 0;
				
				// sweep over outputs
				for(int i=0;i<p_y.length;i++)
				{
					// sweep over inputs
					for(int j=0;j<p_x.length;j++)
					{
						
						/*
						double e =  (2*V*y1[i]* (I1[j]*I1[j]*k*k-alpha*alpha))/((alpha-I1[j]*k)*(alpha-I1[j]*k)) ;
						double a,b;
						a = -Math.log(1+((alpha-I1[j]*k)*y1[i])/(I1[j]*k*C)) ;
						b = (4*I1[j]*k*C*V*alpha/((alpha-I1[j]*k)*(alpha-I1[j]*k)))*Math.log(1+((alpha-I1[j]*k)*y1[i])/(I1[j]*k*C));
						p_y_given_x[i][j] = Math.exp(e+a+b);
						*/
						
						// laplace approximation
						//p_y_given_x[i][j] = Math.exp(-(V)*(k*I1[j]+alpha)*Math.pow((y1[i]-((k*I1[j])/(k*I1[j]+alpha))), 2)/(k*I1[j]));
						// laplace with linear correction
						//p_y_given_x[i][j] = Math.exp(-(V)*((k*I1[j]+alpha)/(k*I1[j]) + k*I1[j]/3.0)*Math.pow((y1[i]-((k*I1[j])/(k*I1[j]+alpha))), 2));
						//double variance = 2+I1[j]*k+(1/I1[j]*k);
						//p_y_given_x[i][j] = Math.exp(-(V)*(2+I1[j]*k+(1/I1[j]*k))*Math.pow((y1[i]-((k*I1[j])/(k*I1[j]+alpha))), 2));
						
						double a = -(2/Otot)-((k*I1[j])/(alpha*Otot))-((alpha)/(k*I1[j]*Otot));
						double b = y1[i]-((k*I1[j]*Otot)/(k*I1[j]+alpha));
						double plap = Math.exp(0.5*V*a*b*b);
						p_y_given_x[i][j] = plap;
						
						/*
						double a = -(2/Otot)-((k*Inp)/(alpha*Otot))-((alpha)/(k*Inp*Otot));
						double b = y-((k*Inp*Otot)/(k*Inp+alpha));
						double plap = Math.exp(0.5*V*a*b*b);
						*/
						
						
						//p_y_x[i][j] = p_y_given_x[i][j]*p_x[j];						// probability chain rule 
						//p_y[i]+= p_y_given_x[i][j]*p_x[j];							// Integrate to find p_y					
						//p_y_x_Norm+=p_y_x[i][j]*dx*dx;

						//}
					}	// for j
					p_y_Norm+= (p_y[i]*dx);
					//System.out.println(y1[i]+" "+p_y[i]);
				}	// end conditional probability loop 
				
				// 2nd conditional probability loop for gathering normalization constants
				for(int i=0;i<p_y.length;i++)
				{
					
					// get conditional output probability
					for(int j=0;j<p_x.length;j++)
					{
						conditionalNorm[i]+=p_y_given_x[j][i];
					}	// for j					
				}	// end conditional probability loop
				
				// Loop for normalizing conditional probability
				for(int i=0;i<p_y.length;i++)
				{
					
					// get conditional output probability
					for(int j=0;j<p_x.length;j++)
					{
						p_y_given_x[j][i]/=(conditionalNorm[i]*dx);						
					}	// for j
					
					//System.out.println(I1[i]+" "+conditionalNorm[i]*dx);
				}	// end conditional probability loop				
				
				
				// p_y loop: to get p_y from p_y|x: p(y=1) = sum(i=1,4) p(y=1|x=i)*p(x=i)
				// and printing loop
				for(int i=0;i<p_y.length;i++)
				{
					for(int j=0;j<p_x.length;j++)
					{
						p_y[i]+=(p_y_given_x[i][j]*p_x[j]*dx);
						//System.out.print(p_y_given_x[i][j]+" ");
					}	// for j
					//System.out.println(p_y[i]);
				}	// end conditional probability loop
				
				//System.exit(0);
				
				// for p_y_x loop				
				for(int i=0;i<p_y.length;i++)
				{
					for(int j=0;j<p_x.length;j++)
					{
						//p_y_x[i][j]=p_y_given_x[j][i]*p_x[j];
						p_y_x[i][j]=p_y_given_x[i][j]*p_x[j];
						//System.out.print(p_y_x[i][j]+" ");
					}	// for j
					//System.out.println();
				}	// 
				
								
				/*
				
				
				for(int i=0;i<p_y.length;i++)
				{
					//System.out.println(p_y[i]);
					p_y[i]/=p_y_Norm;
					//System.out.println(I1[i]+" "+p_y[i]);
					//System.out.println(I1[i]+" "+p_y[i]);
					System.out.println(p_y[i]);
					//System.out.println(p_y_given_x[i][50]);
				}
				//System.out.println(" "+p_y_Norm);
				System.exit(0);
				
				// integrate to find out p_y
				for(int i=0;i<p_y.length;i++)
				{
					for(int j=0;j<p_x.length;j++)
					{
						p_y_x[i][j] /= p_y_x_Norm;
						//System.out.print(p_y_x[i][j]+" ");
						//System.out.println(y1[i]+" "+I1[j]+" "+p_y_x[i][j]);
					}
					//System.out.println();
				}
				//System.exit(0);
				*/
				
				double MI = 0;
				for(int i=0;i<p_y.length;i++)
				{
					for(int j=0;j<p_x.length;j++)
					//for(int j=0;j<i;j++)
					{
						if(p_y_x[i][j]>0 & p_y[i]>0 & p_x[i]>0)
						{
							//MI+=dx*dx*p_y_x[i][j]*Math.log(p_y_x[i][j]/(p_x[j]*p_y[i]));
							MI+=dx*dx*p_y_x[i][j]*Math.log(p_y_x[i][j]/(p_x[j]*p_y[i]));
						}					
					}
				}
				
				if(MI>maxMI)
				{
					maxMI=MI;
				}
					System.out.println(k+" "+MI);
					//System.out.println(MI);
				//}
				//System.out.println(MI);
				
			} // for k
			//System.out.println(V+" "+maxMI);
			//System.out.println(maxMI);
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
