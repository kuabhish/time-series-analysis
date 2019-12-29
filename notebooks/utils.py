import numpy as np

def average(x):
  return [np.mean(x)]*(len(x))

def moving_average(x,n):
  res = [np.nan]*(n-1)
  for i in range(n,len(x)+1):
    res.append(np.mean(x[i-n:i]))
  return res

def double_moving_average(series ,n ):
  res = moving_average(series,n)
  res = moving_average(res,n)
  return res

def single_exponential_smoothing(x , a):
  res = [ np.nan , x[0]]
  for i in range( 2 , len(x) ):
    res.append( a*x[i-1] + (1-a)*res[i-1] )
  return res

def double_exponential_smoothing(x , a=0.5, g=0.5 ):
  res = [x[0]]
  s = [x[0]]
  b = [np.mean(np.sum(x[1:4]) - np.sum(x[0:3]) )]
  for t in range(1,len(x)):
    s.append(a*x[t] + (1-a)*( s[t-1]+b[t-1] ) )
    b.append( g*(s[t] - s[t-1]) + (1-g)*(b[t-1]) )
    res.append(s[t]+b[t])
  return s, res