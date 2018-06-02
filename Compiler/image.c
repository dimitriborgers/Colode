#include "matrix.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define PNG_DEBUG 3
#include <png.h>

struct image {
	int width;
	int height;
	struct mat red;
	struct mat green;
	struct mat blue;
	struct mat alpha;
};

void _image_read(const char* filename, struct image* out) {
	char header[8];
	png_structp png_p;
	png_infop info_p;
	FILE *fp = fopen(filename, "rb");
	if (!fp) {
		printf("ERROR: could not read %s. Make sure the file exists\n", filename);
		abort();
	}
	fread(header, 1, 8, fp);
    if (png_sig_cmp(header, 0, 8)) {
    	printf("ERROR: could not read %s. Make sure the file is a valid .png file.\n", filename);
		abort();
    }
    png_p = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (!png_p) {
    	printf("ERROR: could not read %s. Make sure the file is a valid .png file.\n", filename);
    	abort();
    }
    info_p = png_create_info_struct(png_p);
    if (!info_p) {
    	printf("ERROR: could not read info_strct from %s. Make sure the file is a valid .png file.\n", filename);
    	abort();
    }
    if (setjmp(png_jmpbuf(png_p))) {
    	printf("ERROR: could not read info_strct from %s. Make sure the file is a valid .png file.\n", filename);
    	abort();
    }
    png_init_io(png_p, fp);
    png_set_sig_bytes(png_p, 8);
    png_read_info(png_p, info_p);
    int width = png_get_image_width(png_p, info_p);
    int height = png_get_image_height(png_p, info_p);
    png_byte color_type = png_get_color_type(png_p, info_p);
    png_byte bit_depth = png_get_bit_depth(png_p, info_p);
    int number_of_passes = png_set_interlace_handling(png_p);
    // png_read_update_info(png_p, info_p);
    /* read file */
	if (setjmp(png_jmpbuf(png_p))) {
		printf("ERROR: idk really...");
		abort();
	}

	if(bit_depth == 16)
		png_set_strip_16(png_p);

	if(color_type == PNG_COLOR_TYPE_PALETTE)
		png_set_palette_to_rgb(png_p);

	// PNG_COLOR_TYPE_GRAY_ALPHA is always 8 or 16bit depth.
	if(color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
		png_set_expand_gray_1_2_4_to_8(png_p);

	if(png_get_valid(png_p, info_p, PNG_INFO_tRNS))
		png_set_tRNS_to_alpha(png_p);

	// These color_type don't have an alpha channel then fill it with 0xff.
	if(color_type == PNG_COLOR_TYPE_RGB ||
	 color_type == PNG_COLOR_TYPE_GRAY ||
	 color_type == PNG_COLOR_TYPE_PALETTE)
		png_set_filler(png_p, 0xFF, PNG_FILLER_AFTER);

	if(color_type == PNG_COLOR_TYPE_GRAY ||
	 color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
		png_set_gray_to_rgb(png_p);

	png_read_update_info(png_p, info_p);

	png_bytep* row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
	for (int i=0; i<height; i++)
	        row_pointers[i] = (png_byte*) malloc(png_get_rowbytes(png_p,info_p));

	png_read_image(png_p, row_pointers);
	fclose(fp);
	int size = sizeof(double)*height*width;
	int malloc_count = 0;
	double* channel_d[4];
	while (malloc_count != 4) {
		channel_d[malloc_count] = malloc(size);
		if (channel_d[malloc_count] != NULL) { malloc_count++; }
	}
	for (int i = 0; i < height; ++i)
	{
		png_byte* row = row_pointers[i];
		for (int j = 0; j < width; ++j)
		{
			png_byte* ptr = row + (j*4);
			int idx = _mat_index(width, i, j);
			channel_d[0][idx] = ptr[0];
			channel_d[1][idx] = ptr[1];
			channel_d[2][idx] = ptr[2];
			channel_d[3][idx] = ptr[3];
		}
		free(row);
	}
	free(row_pointers);
	out->width = width;
	out->height = height;
	out->red = (struct mat) {channel_d[0], width, height };
	out->green = (struct mat) {channel_d[1], width, height };
	out->blue = (struct mat) {channel_d[2], width, height };
	out->alpha = (struct mat) {channel_d[3], width, height };
	// _mat_print(&out->channels[1]);
}

void _image_write(const char* filename, struct image* in) {
	int height = in->height;
	int width = in->width;
	FILE *fp = fopen(filename, "wb");
	if(!fp) {
		printf("ERROR: coclose() could not write %s", filename);
		abort();
	}
	png_structp png = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (!png) abort();

	png_infop info = png_create_info_struct(png);
	if (!info) abort();

	if (setjmp(png_jmpbuf(png))) abort();

	png_init_io(png, fp);

	// Output is 16bit depth, RGBA format.
	png_set_IHDR(
		png,
		info,
		width, height,
		16,
		PNG_COLOR_TYPE_RGBA,
		PNG_INTERLACE_NONE,
		PNG_COMPRESSION_TYPE_DEFAULT,
		PNG_FILTER_TYPE_DEFAULT
	);

	png_write_info(png, info);
	png_bytep* row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
	for (int i=0; i < height; i++)
	        row_pointers[i] = (png_byte*) malloc(png_get_rowbytes(png,info));
	// copy from image matrices
	for (int i = 0; i < height; ++i)
	{
		png_byte* row = row_pointers[i];
		for (int j = 0; j < width; ++j)
		{
			png_byte* ptr = row + (j*8);
			int idx = _mat_index(width, i, j);
			((unsigned short*)ptr)[0] = (unsigned short) in->red.d[idx];
			((unsigned short*)ptr)[1] = (unsigned short) in->green.d[idx];
			((unsigned short*)ptr)[2] = (unsigned short) in->blue.d[idx];
			((unsigned short*)ptr)[3] = (unsigned short) in->alpha.d[idx];
			for (int c = 0; c < 4; c++) {
			}
			/*ptr[0] = in->channels[0].d[idx];
			ptr[1] = in->channels[1].d[idx];
			ptr[2] = in->channels[2].d[idx];
			ptr[3] = in->channels[3].d[idx];*/
		}
	}
	png_write_image(png, row_pointers);
	png_write_end(png, NULL);

	for (int i = 0; i < height; i++) {
		free(row_pointers[i]);
	}
	free(row_pointers);
	fclose(fp);
}