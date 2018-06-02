struct mat {
	double* d;
	int width;
	int height;
};

int _mat_index(int width, int i, int j);
void _mat_reset(struct mat* m);
void _mat_zero_out(struct mat* m);
void _mat_print(const struct mat* m);
void _mat_scalar_add(struct mat* m, double a, struct mat* out);
void _mat_scalar_subtract(struct mat* m, double a, struct mat* out);
void _mat_scalar_multiply(struct mat* m, double a, struct mat* out);
void _mat_scalar_divide(struct mat* m, double a, struct mat* out);
void _mat_mat_add(struct mat* l, struct mat* r, struct mat* out);
void _mat_mat_subtract(struct mat* l, struct mat* r, struct mat* out);
void _mat_mat_multiply(struct mat* l, struct mat* r, struct mat* out);
void _mat_make_identity(struct mat* m);
void _mat_mat_inverse(struct mat* l, struct mat* out);
void _mat_mat_power(struct mat* l, int a, struct mat* out);
void _mat_mat_convolute(struct mat* a, struct mat* b, struct mat* out);
int _mat_mat_equal(struct mat* l, struct mat* r);
void _mat_gen_gauss(double sigma, struct mat* out);