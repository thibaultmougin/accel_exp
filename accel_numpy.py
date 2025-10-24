import numpy as np 
from jax import jit 
from functools import wraps
from time import time

def timing(f):
    @wraps(f)
    def wrap(*args, **kw):
        ts = time()
        result = f(*args, **kw)
        te = time()
        print('{:16s} : {:8.3f} µs    (args: {:8} {:8})'.format(f.__name__, 1e6*(te-ts), str(args), str(kw)))
        # print('{:16s} : {:8.3f} µs'.format(f.__name__, 1e6*(te-ts)))
        return result
    return wrap

N_X, N_Y, N_Z = 20, 20, 100
MAT_ZEROS = np.zeros((N_X, N_Y, N_Z), dtype=float)

@timing
def zeros():
    mat = np.zeros((N_X, N_Y, N_Z), dtype=float)

@timing
def empty():
    mat = np.empty((N_X, N_Y, N_Z), dtype=float)

@timing
def zeros_int32():
    mat = np.zeros((N_X, N_Y, N_Z), dtype=np.int32)

@timing
def zeros_int64():
    mat = np.zeros((N_X, N_Y, N_Z), dtype=np.int64)

@timing
def cumulate_maison(n):
    mat = np.ones((N_X, N_Y), dtype=float)
    idx_x = np.arange(0, N_X, n)
    idx_y = np.arange(0, N_Y, n)
    mat_new = np.empty((len(idx_x), len(idx_y)), dtype=float)
    for i, n_i in enumerate(idx_x):
        for j, n_j in enumerate(idx_y):
            mat_new[i,j] = np.sum(mat[n_i:min(n_i+n, N_X), n_j:min(n_j+n, N_Y)])
    return mat_new 

@timing
def cumulate_numpy(n):
    mat = np.ones((N_X, N_Y), dtype=float)
    n_x = N_X + (n-N_X%n)
    n_y = N_Y+ (n-N_Y%n)
    mat_fill = np.zeros((n_x,n_y), dtype=float)
    mat_fill[:N_X, :N_Y] = mat
    mat_fill = mat_fill.reshape(-1,n,n_x//n,n)
    mat_fill = mat_fill.transpose((0,2,1,3))
    mat_new = np.sum(mat_fill, axis=(2,3))
    return mat_new 

@timing
def surfaces_numpy():
    x = np.linspace(0,10,6)
    y = np.linspace(0,10,11)
    s = np.tensordot(x[1:] - x[:-1], y[1:] - y[:-1], axes=0)
    return s 

@timing
def surfaces_maison():
    x = np.linspace(0,10,6)
    y = np.linspace(0,10,11)
    s = np.zeros((len(x)-1, len(y)-1), dtype=float)
    for i in range(x.shape[0]-1):
        for j in range(y.shape[0]-1):
            s[i,j] = (x[i+1] - x[i])*(y[j+1] - y[j])
    return s 

zeros()
empty()
zeros_int32()
zeros_int64()
cumulate_maison(3)
cumulate_numpy(3)
surfaces_maison()
surfaces_numpy()


