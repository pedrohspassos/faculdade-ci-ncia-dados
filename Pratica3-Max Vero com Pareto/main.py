import matplotlib.pyplot as plt
import numpy as np

def main():
    # Be sure the file path is correct
    DATA = get_data("dados_pareto.txt")
    DATA = np.sort(DATA)
    N = len(DATA)

    # Getting a linear space based on the limits of data
    seq = np.linspace(DATA[0], DATA[-1], N)

    # Getting parameters (real parameters used to create dados_pareto.txt)
    x_min = 11.3
    alpha = 2.5

    # Getting parameters from data
    x_min_estimated = get_x_min(DATA)
    alpha_estimated = get_alpha(DATA, N, x_min)

    random_x_min, random_alpha = get_random_parameters(DATA)

    # Applying the pareto distribution p.d.f to all seq in x for both cases
    real_distribution = [pareto_pdf(val, x_min, alpha) for val in seq]
    estimated_distribution = [pareto_pdf(val, x_min_estimated, alpha_estimated) for val in seq]
    random_distribution = [pareto_pdf(val, random_x_min, random_alpha) for val in seq]

    # Plotting both distributions on the same figure
    plt.plot(seq, estimated_distribution, label=f'Estimated Distribution (alpha={alpha_estimated:.2f}, x_min={x_min_estimated:.2f})', c='r')
    plt.plot(seq, real_distribution, label=f'Real Distribution (alpha={alpha:.2f}, x_min={x_min:.2f})', c='b')
    plt.xlabel('x')
    plt.ylabel('Probability Density')
    plt.title('Pareto Distribution')
    plt.legend()
    plt.show()

def get_data(path):
    data = None
    with open(path, "r") as file:
        data = [float(val) for val in file.read().split()]
    return data
    
def pareto_pdf(x, x_min, alpha):
    return (alpha * (x_min ** alpha)) / (x ** (alpha + 1))

def L_func(data, x_min, alpha):
    prod = 1
    for x in data:
        prod *= pareto_pdf(x, x_min, alpha)
    return prod

# Return the real alpha used to generate the dados_pareto.txt and the estimated one
def get_alpha(data, N, x_min):
    return N / np.sum(np.log(data/x_min))

# Return the real x_min used to generate the dados_pareto.txt and the estimated one
def get_x_min(data):
    return data[0]

def get_random_parameters(data):
    random_x_min_values = np.linspace(1.0, 250.0, 100)
    random_alpha_values = np.linspace(1.0, 25.0, 100)

    best_L = 0
    best_x_min = None
    best_alpha = None

    for possible_x_min in random_x_min_values:
        for possible_alpha in random_alpha_values:
            possible_L = L_func(data, possible_x_min, possible_alpha)
            if possible_L > best_L:
                best_L = possible_L
                best_alpha = possible_alpha
                best_x_min = possible_x_min

    return best_x_min, best_alpha

if __name__ == "__main__":
    main()