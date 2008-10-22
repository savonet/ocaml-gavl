/*
  Copyright 2003-2008 Savonet team

  This file is part of Ocaml-gavl.

  Ocaml-gavl is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Ocaml-gavl is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Ocaml-gavl; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>

#include <string.h>
#include <assert.h>
#include <gavl/gavl.h>

/* Video conversion */

CAMLprim value caml_gavl_vid_int_of_define(value d)
{
  CAMLparam1(d);
  char *s = String_val(d); 
  if (!strcmp(s,"GAVL_GRAY_8"))
    CAMLreturn(Val_int(GAVL_GRAY_8)) ;
  if (!strcmp(s,"GAVL_GRAY_16"))
    CAMLreturn(Val_int(GAVL_GRAY_16)) ;
  if (!strcmp(s,"GAVL_GRAY_FLOAT"))
    CAMLreturn(Val_int(GAVL_GRAY_FLOAT)) ;
  if (!strcmp(s,"GAVL_GRAYA_16"))
    CAMLreturn(Val_int(GAVL_GRAYA_16)) ;
  if (!strcmp(s,"GAVL_GRAYA_32"))
    CAMLreturn(Val_int(GAVL_GRAYA_32)) ;
  if (!strcmp(s,"GAVL_GRAYA_FLOAT"))
    CAMLreturn(Val_int(GAVL_GRAYA_FLOAT)) ;
  if (!strcmp(s,"GAVL_RGB_15"))
    CAMLreturn(Val_int(GAVL_RGB_15)) ;
  if (!strcmp(s,"GAVL_BGR_15"))
    CAMLreturn(Val_int(GAVL_BGR_15)) ;
  if (!strcmp(s,"GAVL_RGB_16"))
    CAMLreturn(Val_int(GAVL_RGB_16)) ;
  if (!strcmp(s,"GAVL_BGR_16"))
    CAMLreturn(Val_int(GAVL_BGR_16)) ;
  if (!strcmp(s,"GAVL_RGB_24"))
    CAMLreturn(Val_int(GAVL_RGB_24)) ;
  if (!strcmp(s,"GAVL_BGR_24"))
    CAMLreturn(Val_int(GAVL_BGR_24)) ;
  if (!strcmp(s,"GAVL_RGB_32"))
    CAMLreturn(Val_int(GAVL_RGB_32)) ;
  if (!strcmp(s,"GAVL_BGR_32"))
    CAMLreturn(Val_int(GAVL_BGR_32)) ;
  if (!strcmp(s,"GAVL_RGBA_32"))
    CAMLreturn(Val_int(GAVL_RGBA_32)) ;
  if (!strcmp(s,"GAVL_RGB_48"))
    CAMLreturn(Val_int(GAVL_RGB_48)) ;
  if (!strcmp(s,"GAVL_RGBA_64"))
    CAMLreturn(Val_int(GAVL_RGBA_64)) ;
  if (!strcmp(s,"GAVL_RGB_FLOAT"))
    CAMLreturn(Val_int(GAVL_RGB_FLOAT)) ;
  if (!strcmp(s,"GAVL_RGBA_FLOAT"))
    CAMLreturn(Val_int(GAVL_RGBA_FLOAT)) ;
  if (!strcmp(s,"GAVL_YUY2"))
    CAMLreturn(Val_int(GAVL_YUY2)) ;
  if (!strcmp(s,"GAVL_UYVY"))
    CAMLreturn(Val_int(GAVL_UYVY)) ;
  if (!strcmp(s,"GAVL_YUVA_32"))
    CAMLreturn(Val_int(GAVL_YUVA_32)) ;
  if (!strcmp(s,"GAVL_YUVA_64"))
    CAMLreturn(Val_int(GAVL_YUVA_64)) ;
  if (!strcmp(s,"GAVL_YUV_FLOAT"))
    CAMLreturn(Val_int(GAVL_YUV_FLOAT)) ;
  if (!strcmp(s,"GAVL_YUVA_FLOAT"))
    CAMLreturn(Val_int(GAVL_YUVA_FLOAT)) ;
  if (!strcmp(s,"GAVL_YUV_420_P"))
    CAMLreturn(Val_int(GAVL_YUV_420_P)) ;
  if (!strcmp(s,"GAVL_YUV_422_P"))
    CAMLreturn(Val_int(GAVL_YUV_422_P)) ;
  if (!strcmp(s,"GAVL_YUV_444_P"))
    CAMLreturn(Val_int(GAVL_YUV_444_P)) ;
  if (!strcmp(s,"GAVL_YUV_411_P"))
    CAMLreturn(Val_int(GAVL_YUV_411_P)) ;
  if (!strcmp(s,"GAVL_YUV_410_P"))
    CAMLreturn(Val_int(GAVL_YUV_410_P)) ;
  if (!strcmp(s,"GAVL_YUVJ_420_P"))
    CAMLreturn(Val_int(GAVL_YUVJ_420_P)) ;
  if (!strcmp(s,"GAVL_YUVJ_422_P"))
    CAMLreturn(Val_int(GAVL_YUVJ_422_P)) ;
  if (!strcmp(s,"GAVL_YUVJ_444_P"))
    CAMLreturn(Val_int(GAVL_YUVJ_444_P)) ;
  if (!strcmp(s,"GAVL_YUV_444_P_16"))
    CAMLreturn(Val_int(GAVL_YUV_444_P_16)) ;
  if (!strcmp(s,"GAVL_YUV_422_P_16"))
    CAMLreturn(Val_int(GAVL_YUV_422_P_16)) ;

  caml_failwith("unknown value");
}

static inline int caml_gavl_index_of_format(int pixelformat)
{
  int i = -1;
  switch (pixelformat) {
      case GAVL_GRAY_8: 
        i = 0;
        break;
      case GAVL_GRAY_16: 
        i = 1;
        break;
      case GAVL_GRAY_FLOAT: 
        i = 2;
        break;
      case GAVL_GRAYA_16: 
        i = 3;
        break;
      case GAVL_GRAYA_32: 
        i = 4;
        break;
      case GAVL_GRAYA_FLOAT: 
        i = 5;
        break;
      case GAVL_RGB_15: 
        i = 6;
        break;
      case GAVL_BGR_15: 
        i = 7;
        break;
      case GAVL_RGB_16: 
        i = 8;
        break;
      case GAVL_BGR_16: 
        i = 9;
        break;
      case GAVL_RGB_24: 
        i = 10;
        break;
      case GAVL_BGR_24: 
        i = 11;
        break;
      case GAVL_RGB_32: 
        i = 12;
        break;
      case GAVL_BGR_32: 
        i = 13;
        break;
      case GAVL_RGBA_32: 
        i = 14;
        break;
      case GAVL_RGB_48: 
        i = 15;
        break;
      case GAVL_RGBA_64: 
        i = 16;
        break;
      case GAVL_RGB_FLOAT: 
        i = 17;
        break;
      case GAVL_RGBA_FLOAT: 
        i = 18;
        break;
      case GAVL_YUY2: 
        i = 19;
        break;
      case GAVL_UYVY: 
        i = 20;
        break;
      case GAVL_YUVA_32: 
        i = 21;
        break;
      case GAVL_YUVA_64: 
        i = 22;
        break;
      case GAVL_YUV_FLOAT: 
        i = 23;
        break;
      case GAVL_YUVA_FLOAT: 
        i = 24;
        break;
      case GAVL_YUV_420_P: 
        i = 25;
        break;
      case GAVL_YUV_422_P: 
        i = 26;
        break;
      case GAVL_YUV_444_P: 
        i = 27;
        break;
      case GAVL_YUV_411_P: 
        i = 28;
        break;
      case GAVL_YUV_410_P: 
        i = 29;
        break;
      case GAVL_YUVJ_420_P: 
        i = 30;
        break;
      case GAVL_YUVJ_422_P: 
        i = 31;
        break;
      case GAVL_YUVJ_444_P: 
        i = 32;
        break;
      case GAVL_YUV_444_P_16: 
        i = 33;
        break;
      case GAVL_YUV_422_P_16: 
        i = 34;
        break;
  }

  if (i == -1)
    caml_failwith("Unknown pixelformat");

  return i;
}

static inline gavl_video_format_t *video_format_of_value(value v, gavl_video_format_t *format)
{
  int i = 0;
  format->frame_width      = Int_val(Field(v, i++));
  format->frame_height     = Int_val(Field(v, i++));
  format->image_width      = Int_val(Field(v, i++));
  format->image_height     = Int_val(Field(v, i++));
  format->pixel_width      = Int_val(Field(v, i++));
  format->pixel_height     = Int_val(Field(v, i++));
  format->pixelformat      = Int_val(Field(v, i++));
  format->frame_duration   = Int_val(Field(v, i++));
  format->timescale        = Int_val(Field(v, i++));
  format->framerate_mode   = Int_val(Field(v, i++));
  format->chroma_placement = Int_val(Field(v, i++));
  format->interlace_mode   = Int_val(Field(v, i++));

  return format;
}

static value value_of_format(gavl_video_format_t *format)
{
  CAMLparam0();
  CAMLlocal1(ret);
  ret = caml_alloc_tuple(12);
  int i = 0;
  Store_field(ret,i++,Val_int(format->frame_width));
  Store_field(ret,i++,Val_int(format->frame_height));
  Store_field(ret,i++,Val_int(format->image_width));
  Store_field(ret,i++,Val_int(format->image_height));
  Store_field(ret,i++,Val_int(format->pixel_width));
  Store_field(ret,i++,Val_int(format->pixel_height));
  Store_field(ret,i++,Val_int(caml_gavl_index_of_format(format->pixelformat)));
  Store_field(ret,i++,Val_int(format->frame_duration));
  Store_field(ret,i++,Val_int(format->timescale));
  Store_field(ret,i++,Val_int(format->framerate_mode));
  Store_field(ret,i++,Val_int(format->chroma_placement));
  Store_field(ret,i++,Val_int(format->interlace_mode));

  CAMLreturn(ret);
}

static inline int caml_gavl_bytes_per_line(gavl_video_format_t *format, int plane)
{
  int bytes_per_line;
  int sub_h, sub_v;
  sub_h = 1;
  sub_v = 1;
  bytes_per_line = gavl_pixelformat_is_planar(format->pixelformat) ?
    format->image_width * gavl_pixelformat_bytes_per_component(format->pixelformat) :
    format->image_width * gavl_pixelformat_bytes_per_pixel(format->pixelformat);
  if(plane > 0)
    {
    gavl_pixelformat_chroma_sub(format->pixelformat, &sub_h, &sub_v);
    bytes_per_line /= sub_h;
    }
  return bytes_per_line;
}

/* Computes the size of a plane with index i 
 * Taken from gavl's gavl_video_frame_copy_plane */
static inline int caml_gavl_plane_size(gavl_video_format_t *format, int plane, int stride)
{
  int sub_h, sub_v;
  int height = format->image_height;
  sub_h = 1;
  sub_v = 1;

  if(plane > 0)
    {
    gavl_pixelformat_chroma_sub(format->pixelformat, &sub_h, &sub_v);
    height /= sub_v;
    }

  return height * stride; 
}

static gavl_video_frame_t *caml_gavl_alloc_frame(gavl_video_frame_t *f, gavl_video_format_t *vf)
{
  int p = gavl_pixelformat_num_planes (vf->pixelformat);
  int i,len;
  for (i = 0; i < p; i++)
  {
    len = caml_gavl_bytes_per_line(vf,i);
    f->planes[i]  = malloc(caml_gavl_plane_size(vf,i,len));
    f->strides[i] = len;
    if (f->planes[i] == NULL)
      caml_failwith("malloc");
  }
  f->user_data = NULL;
  f->timestamp = 0;
  f->duration = 0;
  f->interlace_mode = GAVL_INTERLACE_NONE;
  return f;
}

/* Frame here should have been initialized 
 * with the given video format ! */
static gavl_video_frame_t *gavl_video_frame_of_value(value v, gavl_video_format_t *vf, gavl_video_frame_t *f)
{
  int i = 0;
  int j,len,stride;
  struct caml_ba_array *data;
  value tmp,plane;
  gavl_pixelformat_t pf = vf->pixelformat;
  value planes = Field(v, i++);
  int np = gavl_pixelformat_num_planes(pf);
  if (np != Wosize_val(planes))
    caml_raise_constant(*caml_named_value("caml_gavl_invalid_frame"));

  for (j = 0; j < np; j++)
  {
    tmp = Field(planes,j);
    plane = Field(tmp,0);
    stride = Int_val(Field(tmp,1));
    data = Caml_ba_array_val(plane);
    len = caml_gavl_plane_size(vf,j,stride);
    if (data->num_dims != 1 || data->dim[0] != len)
      caml_raise_constant(*caml_named_value("caml_gavl_invalid_frame"));
    f->planes[j] = data->data;
    f->strides[j] = stride;
  }

  f->timestamp      = Int64_val(Field(v,i++));
  f->duration       = Int64_val(Field(v,i++));
  f->interlace_mode = Int_val(Field(v,i++));

  return f;
}

/* Warning: this assumes that the frame has been initialized for the corresponding
 * video format ! 
 * data is *not* copied. */
static value value_of_gavl_video_frame(gavl_video_format_t *vf, gavl_video_frame_t *f)
{
  CAMLparam0();
  CAMLlocal4(v,planes,tmp,p);
  v = caml_alloc_tuple(4);
  int i = 0;
  int j;
  intnat len;
  gavl_pixelformat_t pf = vf->pixelformat;
  int np = gavl_pixelformat_num_planes(pf);
  planes = caml_alloc_tuple(np);
  for (j = 0; j < np; j++)
  {
    if (f->planes[j] == NULL)
      caml_raise_constant(*caml_named_value("caml_gavl_invalid_frame"));
    tmp = caml_alloc_tuple(2);
    len = caml_gavl_plane_size(vf,j,f->strides[j]);
    p = caml_ba_alloc(CAML_BA_MANAGED|CAML_BA_C_LAYOUT|CAML_BA_UINT8,1,f->planes[j],&len);
    Store_field (tmp,0,p);
    Store_field (tmp,1,Val_int(f->strides[j]));
    Store_field (planes,j,tmp);
  }
  Store_field (v, i++, planes);
  Store_field (v, i++, caml_copy_int64(f->timestamp));
  Store_field (v, i++, caml_copy_int64(f->duration));
  Store_field (v, i++, Int_val(f->interlace_mode));

  CAMLreturn(v);
}

typedef struct video_converter_t
{
  /* Converter object */
  gavl_video_converter_t *conv;
  /* Number of needed passes, as returned by init */
  int pass;
  gavl_video_format_t in_vf;
  gavl_video_format_t out_vf;
} vid_conv_t;

#define Vid_conv_val(v) (*((vid_conv_t**)Data_custom_val(v)))

static void finalize_vid_conv(value v)
{
  vid_conv_t *conv = Vid_conv_val(v);
  gavl_video_converter_destroy(conv->conv);
  free(conv);
}

static struct custom_operations vid_conv_ops =
{
  "ocaml_gavl_vid_conv",
  finalize_vid_conv,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value caml_gavl_vid_conv_create(value old, value new)
{
  CAMLparam0();
  CAMLlocal1(ret);
  int pass;
  vid_conv_t *conv = malloc(sizeof(vid_conv_t));
  if (conv == NULL) 
    caml_failwith("malloc");

  conv->conv = gavl_video_converter_create();
  if (conv->conv == NULL)
    caml_failwith("gavl_video_converter_create");
  video_format_of_value(old,&conv->in_vf);
  video_format_of_value(new,&conv->out_vf);
  pass = gavl_video_converter_init(conv->conv,&conv->in_vf,&conv->out_vf);
  
  if (pass == -1)
  {
    gavl_video_converter_destroy(conv->conv);
    free(conv);
    caml_raise_constant(*caml_named_value("caml_gavl_invalid_conversion"));
  }

  conv->pass  = pass;

  ret = caml_alloc_custom(&vid_conv_ops, sizeof(vid_conv_t *), 1, 0);
  Vid_conv_val(ret) = conv;

  CAMLreturn(ret);
}

CAMLprim value caml_gavl_vid_conv_init(value conv, value old, value new)
{
  CAMLparam1(conv);
  vid_conv_t *vid_conv = Vid_conv_val(conv);
  video_format_of_value(old,&vid_conv->in_vf);
  video_format_of_value(new,&vid_conv->out_vf);
  int pass = gavl_video_converter_init(vid_conv->conv,&vid_conv->in_vf,&vid_conv->out_vf);

  if (pass == -1)
    caml_raise_constant(*caml_named_value("caml_gavl_invalid_conversion"));
  
  CAMLreturn(Val_unit);
}

CAMLprim value caml_gavl_vid_conv_get_formats(value conv)
{
  CAMLparam1(conv);
  CAMLlocal1(ret);
  vid_conv_t *vid_conv = Vid_conv_val(conv);
  ret = caml_alloc_tuple(2);
  Store_field(ret,0,value_of_format(&vid_conv->in_vf));
  Store_field(ret,1,value_of_format(&vid_conv->out_vf));

  CAMLreturn(ret);
}

CAMLprim value caml_gavl_vid_conv_convert(value conv, value old, value new) 
{
  vid_conv_t *vid_conv = Vid_conv_val(conv);

  /* pass < 0 should not happen since it 
     cannot be instanciated like that..*/
  assert(vid_conv->pass >= 0);

  gavl_video_converter_t *cnv = vid_conv->conv;
  gavl_video_frame_t inf;
  gavl_video_frame_t outf;
  int j;
  gavl_video_frame_of_value(old,&vid_conv->in_vf,&inf);
  gavl_video_frame_of_value(new,&vid_conv->out_vf,&outf);

  /* pass == 0 means no conversion is needed.. */ 
  if (vid_conv->pass == 0)
    caml_raise_constant(*caml_named_value("caml_gavl_no_conversion_needed"));

  caml_register_global_root(&old);
  caml_register_global_root(&new);
  caml_register_global_root(&conv);

  caml_enter_blocking_section();
  for (j = 0; j < vid_conv->pass; j++)
    gavl_video_convert(cnv,&inf,&outf);
  caml_leave_blocking_section();  

  caml_remove_global_root(&old);
  caml_remove_global_root(&new);
  caml_remove_global_root(&conv);

  return Val_unit;
}

CAMLprim value caml_gavl_vid_conv_new_frame(value format)
{
  CAMLparam1(format);
  CAMLlocal1(ret);
  gavl_video_format_t vf;
  video_format_of_value(format,&vf); 
  gavl_video_frame_t frame;
  caml_gavl_alloc_frame(&frame,&vf);
  ret = value_of_gavl_video_frame(&vf,&frame);
  CAMLreturn(ret);
}

static inline gavl_video_options_t *caml_gavl_vid_conv_opt(value conv)
{
  vid_conv_t *vid_conv = Vid_conv_val(conv);
  return gavl_video_converter_get_options(vid_conv->conv);
}

static inline gavl_rectangle_f_t *caml_gavl_f_rect_of_val(value v, gavl_rectangle_f_t *rec)
{
  int i = 0;
  rec->x = Double_val(Field(v,i++));
  rec->y = Double_val(Field(v,i++));
  rec->w = Double_val(Field(v,i++));
  rec->h = Double_val(Field(v,i++));

  return rec;
}

CAMLprim value caml_gavl_val_of_f_rect(gavl_rectangle_f_t *r)
{
  CAMLparam0();
  CAMLlocal1(ret);
  ret = caml_alloc_tuple(4);
  int i = 0;
  Store_field(ret,i++,caml_copy_double(r->x));
  Store_field(ret,i++,caml_copy_double(r->y));
  Store_field(ret,i++,caml_copy_double(r->w));
  Store_field(ret,i++,caml_copy_double(r->h));
  
  CAMLreturn(ret);
}

static inline gavl_rectangle_i_t *caml_gavl_i_rect_of_val(value v, gavl_rectangle_i_t *rec)
{
  int i = 0;
  rec->x = Int_val(Field(v,i++));
  rec->y = Int_val(Field(v,i++));
  rec->w = Int_val(Field(v,i++));
  rec->h = Int_val(Field(v,i++));

  return rec;
}

CAMLprim value caml_gavl_val_of_i_rect(gavl_rectangle_i_t *r)
{
  CAMLparam0();
  CAMLlocal1(ret);
  ret = caml_alloc_tuple(4);
  int i = 0;
  Store_field(ret, i++, Val_int(r->x));
  Store_field(ret, i++, Val_int(r->y));
  Store_field(ret, i++, Val_int(r->w));
  Store_field(ret, i++, Val_int(r->h));

  CAMLreturn(ret);
}

CAMLprim value caml_gavl_vid_conv_reinit(value conv)
{
  CAMLparam1(conv);
  vid_conv_t *vid_conv = Vid_conv_val(conv);
  gavl_video_converter_reinit(vid_conv->conv);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_gavl_vid_conv_set_quality(value conv, value q)
{
  CAMLparam1(conv);
  gavl_video_options_set_quality(caml_gavl_vid_conv_opt(conv),Int_val(q));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_gavl_vid_conv_get_quality(value conv)
{
  CAMLparam1(conv);
  CAMLreturn(Val_int(gavl_video_options_get_quality(caml_gavl_vid_conv_opt(conv))));
}

CAMLprim value caml_gavl_vid_conv_set_rectangle(value conv, value s_rec, value d_rec)
{
  CAMLparam3(conv,s_rec,d_rec);
  gavl_rectangle_f_t src;
  gavl_rectangle_i_t dst;  
  gavl_video_options_set_rectangles(caml_gavl_vid_conv_opt(conv),
                                    caml_gavl_f_rect_of_val(s_rec,&src),
                                    caml_gavl_i_rect_of_val(d_rec,&dst));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_gavl_vid_conv_get_rectangle(value conv)
{
  CAMLparam1(conv);
  CAMLlocal1(ret);
  gavl_rectangle_f_t src;
  gavl_rectangle_i_t dst;
  gavl_video_options_get_rectangles(caml_gavl_vid_conv_opt(conv),&src,&dst);
  ret = caml_alloc_tuple(2);
  Store_field(ret,0,caml_gavl_val_of_f_rect(&src));
  Store_field(ret,1,caml_gavl_val_of_i_rect(&dst));
  CAMLreturn(ret);
}

