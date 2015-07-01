
// comment the package in order to compile codes
// because the algs4 is defined in default package
//package learn.algorithms.percolate;

/**
 * Created by vulcann on 30/06/15.
 */
public class Percolation {

    /**
     * Initializes an percolation with N x N grid where all sites are blocked
     *
     * The function throws IllegalArgumentException if N < 0
     *
     * @param N the length of grid
     */
    public Percolation(int N) {
        if (N < 0) {
            throw new IllegalArgumentException("N must > 0, N: " + N);
        }
        this.N = N;
        sites = new boolean [N*N+2];
        for (int i = 0; i < sites.length; ++i) {
            sites[i] = false;
        }
        sites[0] = true; // the virtual top site
        sites[1] = true; // the virtual bottom site
        uf = new QuickFindUF(N*N+2);
    }

    /**
     * Open site (row i, column j) if it is not open yet
     *
     * It throws IndexOutOfBoundsException if i or j is beyond its prescribed range (1, N)
     *
     * @param i the row index, starting from 1
     * @param j the colum index, starting from 1
     */
    public void open(int i, int j) {
        validate(i);
        validate(j);
        int q = getArrayIndex(i, j);
        if (!sites[q]) {
            sites[q] = true;
            // find neightbors
            if (i == 1) {
                uf.union(q, 0);
            }
            if (i == N) {
                uf.union(q, 1);
            }
            if (i > 1 && isOpen(i-1, j)) {
                int topNeighborIdx = getArrayIndex(i-1, j);
                uf.union(q, topNeighborIdx);
            }
            if (i < N && isOpen(i+1, j)) {
                int botNeighborIdx = getArrayIndex(i+1, j);
                uf.union(q, botNeighborIdx);
            }
            if (j > 1 && isOpen(i, j-1)) {
                int leftNeighborIdx = getArrayIndex(i, j-1);
                uf.union(q, leftNeighborIdx);
            }
            if (j < N && isOpen(i, j+1)) {
                int rightNeighborIdx = getArrayIndex(i, j+1);
                uf.union(q, rightNeighborIdx);
            }
        }
    }

    /**
     * Is the site (row i, column j) open ?
     *
     * It throws IndexOutOfBoundsException if i or j is beyond its prescribed range (1, N)
     *
     * @param i the row index, starting from 1
     * @param j the colum index, starting from 1
     * @return true if the site with given indices is open
     */
    public boolean isOpen(int i, int j) {
        validate(i);
        validate(j);
        int q = getArrayIndex(i, j);
        return sites[q];
    }

    /**
     * Is the site (row i, column j) full ? (connected to an open site in the top row)
     *
     * It throws IndexOutOfBoundsException if i or j is beyond its prescribed range (1, N)
     *
     * @param i the row index, starting from 1
     * @param j the colum index, starting from 1
     * @return true if the side with given indices is full
     */
    public boolean isFull(int i, int j) {
        validate(i);
        validate(j);
        int q = getArrayIndex(i, j);
        return sites[q] && uf.connected(0, q);
    }

    /**
     * check the percolation
     *
     * @return true if it percolates
     */
    public boolean percolates() {
        return uf.connected(0, 1);
    }

    // validate that p is a valid index
    private void validate(int p) {
        if (p < 0 || p >= N) {
            throw new IndexOutOfBoundsException("index " + p + " is not between 0 and " + N);
        }
    }

    /**
     * sites[0] the virtual top element;
     * sites[1] the virtual bottom element;
     */
    private int getArrayIndex(int i, int j) {
        return (N-1) * i + j + 1;
    }

    private final boolean[] sites;
    private final int N;
    private QuickFindUF uf;

    public static void main(String[] args) {

    }
}
