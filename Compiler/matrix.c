#include <stdio.h>
#include <math.h>
#include <string.h>
#include "matrix.h"


int _mat_index(int width, int i, int j) {
	return i*width + j;
}

void _mat_reset(struct mat* m) {
	m->width = 0;
	m->height = 0;
}

void _mat_zero_out(struct mat* m) {
	int size = m->height*m->width;
	for (int i = 0; i < size; ++i)
	{
		m->d[i] = 0.0;
	}
}

void _mat_print(const struct mat* m) {
	printf("%d x %d [", m->height, m->width);
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width,i,j);
			printf("%03.2f ", m->d[idx]);
		}
		if (i != (m->height-1)) printf("|\n");
	}
	printf(" ]\n");
}

void _mat_scalar_add(struct mat* m, double a, struct mat* out) {
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width,i,j);
			out->d[idx] = m->d[idx] + a;
		}
	}
}

void _mat_scalar_subtract(struct mat* m, double a, struct mat* out) {
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width,i,j);
			out->d[idx] = m->d[idx] - a;
		}
	}
}

void _mat_scalar_multiply(struct mat* m, double a, struct mat* out) {
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width,i,j);
			out->d[idx] = m->d[idx] * a;
		}
	}
}

void _mat_scalar_divide(struct mat* m, double a, struct mat* out) {
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width,i,j);
			out->d[idx] = m->d[idx] / a;
		}
	}
}

void _mat_mat_add(struct mat* l, struct mat* r, struct mat* out) {
	if ((l->width != r->width) || (l->height != r->height)) {
		printf("ERROR: Illegal attempt to add %dx%d matrix and %dx%d matrix.", l->height, l->width, r->height, r->width);
		_mat_reset(out);
	}
	else {
		for (int i = 0; i < l->height; ++i)
		{
			for (int j = 0; j < l->width; ++j)
			{
				int idx = _mat_index(l->width,i,j);
				out->d[idx] = l->d[idx] + r->d[idx];
			}
		}
	}
}
void _mat_mat_subtract(struct mat* l, struct mat* r, struct mat* out) {
	if ((l->width != r->width) || (l->height != r->height)) {
		printf("ERROR: Illegal attempt to add %dx%d matrix and %dx%d matrix. Operation skipped and output ", l->height, l->width, r->height, r->width);
		_mat_reset(out);
	}
	else {
		for (int i = 0; i < l->height; ++i)
		{
			for (int j = 0; j < l->width; ++j)
			{
				int idx = _mat_index(l->width,i,j);
				out->d[idx] = l->d[idx] - r->d[idx];
			}
		}
	}
}
void _mat_mat_multiply(struct mat* l, struct mat* r, struct mat* out) {
	if (l->width != r->height) {
		printf("ERROR: Illegal attempt to multiply %dx%d matrix and %dx%d matrix. The second operand should have been %dx%d",
			l->height, l->width, r->height, r->width, l->width, r->width);
		_mat_reset(out);
		return;
	}
	int size = l->height*r->width;
	double d[size];
	struct mat copy = {d, r->width, l->height};
	for (int i = 0; i < l->height; ++i)
	{
		for (int j = 0; j < r->width; ++j)
		{
			double sum = 0;
			for (int k = 0; k < r->height; ++k) {
				int l_idx = _mat_index(l->width,i,k);
				int r_idx = _mat_index(r->width,k,j);
				sum +=  l->d[l_idx] * r->d[r_idx];
			}
			int idx = _mat_index(copy.width, i, j);
			copy.d[idx] = sum;
		}
	}
	for (int i = 0; i < size; ++i) {
		out->d[i] = copy.d[i];
	}
}

int _mat_mat_equal(struct mat* l, struct mat* r) {
	if ((l->width != r->width) || (l->height != r->height)) {
		return 0;
	}
	int equal = 1;
	for (int i = 0; i < l->height; ++i)
	{
		for (int j = 0; j < r->width; ++j)
		{
			int idx = _mat_index(l->width, i, j);
			if (l->d[idx] != r->d[idx]) {
				equal = 0;
			}
		}
	}
	return equal;
}
void _mat_make_identity(struct mat* m) {
	int count = 0;
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width, i, j);
			if (j == i) {
				m->d[idx] = 1.0;
				count++;
			} else {
				m->d[idx] = 0.0;
			}
		}
	}
}

void _mat_mat_inverse(struct mat* l, struct mat* out) {
	double rcond = 1E-15; // numpy default
}
void _mat_mat_power(struct mat* l, int a, struct mat* out) {
	if (l->width != l->height) {
		printf("ERROR: cannot take the power of a non-square matrix.\n");
		return;
	}
	int i_len = l->width;
	double d[i_len * i_len]; 
	struct mat ident = {d, i_len, i_len};
	_mat_make_identity(&ident);
	if (a == 0) {
		out->width = ident.width;
		out->height = ident.height;
		memcpy(out->d, ident.d, i_len * i_len);
		return;
	}
	_mat_mat_multiply(l, &ident, out);
	for (int i = 1; i < a; ++i)
	{
		_mat_mat_multiply(l, out, out);
	}
	if (a < 0) {
		_mat_mat_inverse(out, out);
	}
}

int convolve2D(struct mat* a, struct mat* kernel, struct mat* out) {
	int i, j, m, n;
    double *in_p, *in_p2, *out_p, *out_p2, *kern_p;
    int kern_center_x, kern_center_y;
    int row_min, row_max;                             // to check boundary of input array
    int col_min, col_max; 
    int a_x = a->width;
    int a_y = a->height;
    int kern_x = kernel->width;
    int kern_y = kernel->height;
    kern_center_x = kern_x >> 1; // half of kern width
    kern_center_y = kern_y >> 1; // half of kern width
    in_p = in_p2 = a->d + _mat_index(a_x, kern_center_y, kern_center_x);
    out_p = out->d;
    kern_p = kernel->d;

    for (int i = 0; i < a_y; ++i){
    	row_max = i + kern_center_y;
    	row_min = i - a_y + kern_center_y;
    	for (int j = 0; j < a_x; ++j) {
    		col_max = j + kern_center_x;
    		col_min = j - a_x + kern_center_x;
    		*out_p = 0;
    		// flip kernel and multiply
    		for (int l = 0; l < kern_y; ++l) {
    			if (l <= row_max && l > row_min) {
    				for (m = 0; m < kern_x; ++m) {
    					if (m <= col_max && m > col_min) {
    						*out_p += *(in_p - m) * (*kern_p);
    					}
    					kern_p++;
    				}
    			} else {
    				kern_p += kern_x;
    			}
    			in_p -= a_x;
    		}
    		if (*out_p < 0) *out_p = 0;
    		kern_p = kernel->d;
    		++in_p2;
    		in_p = in_p2;
    		++out_p;
    	}
    }
    // for (int i = 0; i < a_x * a_y ; ++i)
    //  {
    //  	out->d[i] = 0xe;
    //  } 
    //  _mat_print(out);
    return 1;
}
void _mat_mat_convolute(struct mat* a, struct mat* b, struct mat* out) {
	convolve2D(a, b, out);
}

void _mat_gen_gauss(double sigma, struct mat* out) {
	double r, s = 2.0 * sigma * sigma;
	double sum = 0.0;
 	double half_i = floor(out->width / 2);
 	double half_j = floor(out->height / 2);
    // generating 5x5 kernel
    for (int i = 0; i < out->width; i++)
    {
        for(int j = 0; j < out->height; j++)
        {
            r = sqrt(pow(i-half_i, 2) + pow(j-half_j, 2));
            int idx = _mat_index(out->width, i, j);
            out->d[idx] = (exp(-(r*r)/s))/(M_PI * s);
            sum += out->d[idx];
        }
    }
 
    // normalising the Kernel
    for (int i = 0; i < out->width; ++i)
        for (int j = 0; j < out->height; ++j) {
        	out->d[_mat_index(out->width, i, j)] /= sum;
        }
}	

void _mat_gen_sharpen(struct mat* out) {
    int mid_idx = _mat_index(out->width, out->width / 2, out->height / 2);
    out->d[mid_idx] = 5;
    out->d[_mat_index(out->width, out->width / 2, 0)] = -1.0;
    out->d[_mat_index(out->width, 0, out->height / 2)] = -1.0;
    out->d[_mat_index(out->width, out->width - 1, out->height / 2)] = -1.0;
    out->d[_mat_index(out->width, out->width / 2, out->height -1)] = -1.0;
}
void _mat_gen_brighten(double value, struct mat* out) {
	int mid_idx = _mat_index(out->width, out->width / 2, out->height / 2);
	out->d[mid_idx] = value;
}
void _mat_gen_edge_detect(struct mat* out) {
	for (int i = 0; i < out->width; i++)
    {
        for(int j = 0; j < out->height; j++)
        {
            int idx = _mat_index(out->width, i, j);
            out->d[idx] = -1.0;
        }
    }
    int mid_idx = _mat_index(out->width, out->width / 2, out->height / 2);
    out->d[mid_idx] = 8.0;
}