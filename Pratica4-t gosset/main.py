import numpy as np
import matplotlib.pyplot as plt

def main():
  muA = 8.0
  sigmaA = 2.0
  nA = 50
  muB = 9.5
  sigmaB = 1.0
  nB = 45

  # A == B
  plot_difference_histogram(muA, sigmaA, nA, muA, sigmaA, nB) 

  # Diff calculeted with Gosset proposal (weighted)
  #plot_difference_histogram(muA, sigmaA, nA, muB, sigmaB, nB, True)

  # A == B + Diff calculeted with Gosset proposal (weighted)
  #plot_difference_histogram(muA, sigmaA, nA, muA, sigmaA, nB, True) 

def calculate_integral(dados):
  hist_values, bin_edges = np.histogram(dados, bins=50, density=True)
  delta = bin_edges[1] - bin_edges[0]
  print(delta)
  mid_index = int(len(hist_values)/2)
  print(mid_index)
  current_sum = hist_values[mid_index] * delta
  print(current_sum)
  target_percentage = 0.95
  i = 1

  while current_sum < target_percentage:
    current_sum += (hist_values[mid_index - i] * delta) + (hist_values[mid_index + i] * delta)
    i += 1

  left_index = mid_index - (i - 1)
  right_index = mid_index + (i - 1)
    
  print(f"Intervalo = [{bin_edges[left_index]:.6f}, {bin_edges[right_index]:.6f}]")
  print(f"Soma = {current_sum}")

  plt.axvline(bin_edges[left_index], color='r', linestyle='dashed', linewidth=2, label='Left Edge')
  plt.axvline(bin_edges[right_index], color='g', linestyle='dashed', linewidth=2, label='Right Edge')

def calculate_diff(muA, sigmaA, nA, muB, sigmaB, nB):
  S = int(1E6)
  D = np.full(S, np.inf)

  for i in range(S):
    a = np.random.normal(muA, sigmaA, nA)
    b = np.random.normal(muB, sigmaB, nB)
    D[i] = np.mean(a) - np.mean(b)

  return D

def calculate_diff_with_weight(muA, sigmaA, nA, muB, sigmaB, nB):
  S = int(1E6)
  D = np.full(S, np.inf)

  for i in range(S):
    a = np.random.normal(muA, sigmaA, nA)
    b = np.random.normal(muB, sigmaB, nB)

    weight = 1 / np.sqrt( sigmaA**2 / nA  + sigmaB**2 / nB )
    D[i] = (np.mean(a) - np.mean(b)) * weight

  return D

def plot_histogram(D):
  plt.hist(D, bins=50)
  plt.title("Histogram of D")
  plt.legend()
  plt.show()

def plot_difference_histogram(muA, sigmaA, nA, muB, sigmaB, nB, hasWeight=False):
  A = np.random.normal(muA, sigmaA, nA)
  B = np.random.normal(muB, sigmaB, nB)

  muA_est = np.mean(A)
  sigmaA_est = np.std(A)

  muB_est = np.mean(B)
  sigmaB_est = np.std(B)

  if hasWeight is True:
    D = calculate_diff_with_weight(muA_est, sigmaA_est, nA, muB_est, sigmaB_est, nB)
  else:
    D = calculate_diff(muA_est, sigmaA_est, nA, muB_est, sigmaB_est, nB)

  calculate_integral(D)
  #plot_histogram(D)

if __name__ == "__main__":
  main()