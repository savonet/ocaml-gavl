(*
 * Copyright 2003-2008 Savonet team
 *
 * This file is part of Ocaml-gavl.
 *
 * Ocaml-gavl is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-gavl is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-gavl; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module Video = 
struct

  exception Invalid_frame
  exception Invalid_conversion
  exception Not_implemented

  let _ = 
    Callback.register_exception "caml_gavl_invalid_frame" Invalid_frame;
    Callback.register_exception "caml_gavl_invalid_conversion" Invalid_conversion

  type interlace_mode = 
   | No_interlace (* Progressive *) 
   | Top_first    (* Top field first *)
   | Bottom_first (* Bottom field first *)
   | Mixed        (* Use interlace_mode of the frames *)

  type pixel_format = 
   | Gray_8       (* 8 bit gray, scaled 0x00..0xff *)
   | Gray_16      (* 16 bit gray, scaled 0x0000..0xffff *)
   | Gray_float   (* floating point gray, scaled 0.0..1.0 *)
   | Graya_16     (* 8 bit gray + alpha, scaled 0x00..0xff *)
   | Graya_32     (* 16 bit gray + alpha, scaled 0x0000..0xffff *)
   | Graya_float  (* floating point gray + alpha, scaled 0.0..1.0 *)
   | Rgb_15       (* 15 bit RGB. Each pixel is a uint16_t in native byte order. 
                   * Color masks are: for red: 0x7C00, for green: 0x03e0, for blue: 0x001f *)
   | Bgr_15       (* 15 bit BGR. Each pixel is a uint16_t in native byte order. 
                   * Color masks are: for red: 0x001f, for green: 0x03e0, for blue: 0x7C00 *)
   | Rgb_16       (* 16 bit RGB. Each pixel is a uint16_t in native byte order. 
                   * Color masks are: for red: 0xf800, for green: 0x07e0, for blue: 0x001f *)
   | Bgr_16       (* 16 bit BGR. Each pixel is a uint16_t in native byte order. 
                   * Color masks are: for red: 0x001f, for green: 0x07e0, for blue: 0xf800 *)
   | Rgb_24       (* 24 bit RGB. Each color is an uint8_t. Color order is RGBRGB *)
   | Bgr_24       (* 24 bit BGR. Each color is an uint8_t. Color order is BGRBGR *)
   | Rgb_32       (* 32 bit RGB. Each color is an uint8_t. Color order is RGBXRGBX, where X is unused *)
   | Bgr_32       (* 32 bit BGR. Each color is an uint8_t. Color order is BGRXBGRX, where X is unused *)
   | Rgba_32      (* 32 bit RGBA. Each color is an uint8_t. Color order is RGBARGBA *)
   | Rgb_48       (* 48 bit RGB. Each color is an uint16_t in native byte order. Color order is RGBRGB *)
   | Rgba_64      (* 64 bit RGBA. Each color is an uint16_t in native byte order. Color order is RGBARGBA *)
   | Rgb_float    (* float RGB. Each color is a float (0.0 .. 1.0) in native byte order. Color order is RGBRGB *)
   | Rgba_float   (* float RGBA. Each color is a float (0.0 .. 1.0) in native byte order. Color order is RGBARGBA *)
   | Yuy2         (* Packed YCbCr 4:2:2. Each component is an uint8_t. Component order is Y1 U1 Y2 V1 *)
   | Yuvy         (* Packed YCbCr 4:2:2. Each component is an uint8_t. Component order is U1 Y1 V1 Y2 *)
   | Yuva_32      (* Packed YCbCrA 4:4:4:4. Each component is an uint8_t. 
                   * Component order is YUVA. Luma and chroma are video scaled, alpha is 0..255. *)
   | Yuva_64      (* Packed YCbCrA 4:4:4:4. Each component is an uint16_t. 
                   * Component order is YUVA. Luma and chroma are video scaled, alpha is 0..65535. *)
   | Yuv_float    (* Packed YCbCr 4:4:4. Each component is a float. Luma is scaled 0.0..1.0, chroma is -0.5..0.5 *)
   | Yuva_float   (* Packed YCbCrA 4:4:4:4. Each component is a float. Luma is scaled 0.0..1.0, chroma is -0.5..0.5 *)
   | Yuv_420_p    (* Packed YCbCrA 4:4:4:4. Each component is an uint16_t. 
                   * Component order is YUVA. Luma and chroma are video scaled, alpha is 0..65535. *)
   | Yuv_422_p    (* Planar YCbCr 4:2:2. Each component is an uint8_t *)
   | Yuv_444_p    (* Planar YCbCr 4:4:4. Each component is an uint8_t *)
   | Yuv_411_p    (* Planar YCbCr 4:1:1. Each component is an uint8_t *)
   | Yuv_410_p    (* Planar YCbCr 4:1:0. Each component is an uint8_t *)
   | Yuvj_420_p   (* Planar YCbCr 4:2:0. Each component is an uint8_t, 
                   * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuvj_422_p   (* Planar YCbCr 4:2:2. Each component is an uint8_t, 
                   * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuvj_444_p   (* Planar YCbCr 4:4:4. Each component is an uint8_t, 
                   * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuv_444_p_16 (* 16 bit Planar YCbCr 4:4:4. Each component is an uint16_t in native byte order. *)
   | Yuv_422_p_16 (* 16 bit Planar YCbCr 4:2:2. Each component is an uint16_t in native byte order. *)

  let is_float f =
    match f with
      | Gray_float
      | Graya_float
      | Rgb_float
      | Rgba_float
      | Yuv_float
      | Yuva_float -> true
      | _ -> false

  let is_int16 f =
    match f with
      | Gray_16
      | Graya_32
      | Rgb_15
      | Bgr_15
      | Rgb_16
      | Bgr_16
      | Rgb_48
      | Rgba_64
      | Yuva_64
      | Yuv_420_p
      | Yuv_444_p_16
      | Yuv_422_p_16 -> true
      | _ -> false

  let is_int8 f = not (is_float f) && not (is_int16 f)

  type framerate_mode = 
   | Constant  (* Constant framerate *)
   | Variable  (* Variable framerate *)
   | Still     (* Still image        *)

  type chroma_placement = 
   | Default (* MPEG-1/JPEG *)
   | Mpeg2   (* MPEG-2      *)
   | Dvpal   (* DV PAL      *)

  type format = 
  { 
    frame_width      : int;
    frame_height     : int;
    image_width      : int;  
    image_height     : int;
    pixel_width      : int;
    pixel_height     : int;
    pixelformat      : pixel_format;
    frame_duration   : int;
    timescale        : int;
    framerate_mode   : framerate_mode;
    chroma_placement : chroma_placement;
    interlace_mode   : interlace_mode
  }

  type internal_format =
  {
    _frame_width      : int;
    _frame_height     : int;
    _image_width      : int;
    _image_height     : int;
    _pixel_width      : int;
    _pixel_height     : int;
    _pixelformat      : int;
    _frame_duration   : int;
    _timescale        : int;
    _framerate_mode   : framerate_mode;
    _chroma_placement : chroma_placement;
    _interlace_mode   : interlace_mode
  }

  external int_of_define : string -> int = "caml_gavl_vid_int_of_define"

  let int_of_pf x = 
    match x with
      | Gray_8       -> int_of_define "GAVL_GRAY_8"
      | Gray_16      -> int_of_define "GAVL_GRAY_16"
      | Gray_float   -> int_of_define "GAVL_GRAY_FLOAT"
      | Graya_16     -> int_of_define "GAVL_GRAYA_16"
      | Graya_32     -> int_of_define "GAVL_GRAYA_32"
      | Graya_float  -> int_of_define "GAVL_GRAYA_FLOAT"
      | Rgb_15       -> int_of_define "GAVL_RGB_15"
      | Bgr_15       -> int_of_define "GAVL_BGR_15"
      | Rgb_16       -> int_of_define "GAVL_RGB_16"
      | Bgr_16       -> int_of_define "GAVL_BGR_16"
      | Rgb_24       -> int_of_define "GAVL_RGB_24"
      | Bgr_24       -> int_of_define "GAVL_BGR_24"
      | Rgb_32       -> int_of_define "GAVL_RGB_32"
      | Bgr_32       -> int_of_define "GAVL_BGR_32"
      | Rgba_32      -> int_of_define "GAVL_RGBA_32"
      | Rgb_48       -> int_of_define "GAVL_RGB_48"
      | Rgba_64      -> int_of_define "GAVL_RGBA_64"
      | Rgb_float    -> int_of_define "GAVL_RGB_FLOAT"
      | Rgba_float   -> int_of_define "GAVL_RGBA_FLOAT"
      | Yuy2         -> int_of_define "GAVL_YUY2"
      | Yuvy         -> int_of_define "GAVL_UYVY"
      | Yuva_32      -> int_of_define "GAVL_YUVA_32"
      | Yuva_64      -> int_of_define "GAVL_YUVA_64"
      | Yuv_float    -> int_of_define "GAVL_YUV_FLOAT"
      | Yuva_float   -> int_of_define "GAVL_YUVA_FLOAT"
      | Yuv_420_p    -> int_of_define "GAVL_YUV_420_P"
      | Yuv_422_p    -> int_of_define "GAVL_YUV_422_P"
      | Yuv_444_p    -> int_of_define "GAVL_YUV_444_P"
      | Yuv_411_p    -> int_of_define "GAVL_YUV_411_P"
      | Yuv_410_p    -> int_of_define "GAVL_YUV_410_P"
      | Yuvj_420_p   -> int_of_define "GAVL_YUVJ_420_P"
      | Yuvj_422_p   -> int_of_define "GAVL_YUVJ_422_P"
      | Yuvj_444_p   -> int_of_define "GAVL_YUVJ_444_P"
      | Yuv_444_p_16 -> int_of_define "GAVL_YUV_444_P_16"
      | Yuv_422_p_16 -> int_of_define "GAVL_YUV_422_P_16"

  let internal_format_of_format f =
  if not (is_int8 f.pixelformat) then
    raise Not_implemented; 
  {
    _frame_width      = f.frame_width;
    _frame_height     = f.frame_height;
    _image_width      = f.image_width;
    _image_height     = f.image_height;
    _pixel_width      = f.pixel_width;
    _pixel_height     = f.pixel_height;
    _pixelformat      = int_of_pf f.pixelformat;
    _frame_duration   = f.frame_duration;
    _timescale        = f.timescale;
    _framerate_mode   = f.framerate_mode;
    _chroma_placement = f.chroma_placement;
    _interlace_mode   = f.interlace_mode
  }

  type plane = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type frame = 
  {
    planes               : (plane*int) array;
    timestamp            : Int64.t;
    duration             : Int64.t; 
    frame_interlace_mode : interlace_mode
  }

  external new_frame : internal_format -> frame = "caml_gavl_vid_conv_new_frame"
  let new_frame fmt = new_frame (internal_format_of_format fmt)

  external clear_frame : internal_format -> frame -> unit = "caml_gavl_vid_conv_frame_clear"
  let clear_frame fmt f = clear_frame (internal_format_of_format fmt) f

  type t

  external create_converter : internal_format -> internal_format -> t = "caml_gavl_vid_conv_create"

  let create_converter f g = 
    create_converter (internal_format_of_format f)
                     (internal_format_of_format g)

  external init : t -> internal_format -> internal_format -> unit = "caml_gavl_vid_conv_init"

  let init c f g = 
    init c (internal_format_of_format f)
           (internal_format_of_format g)

  external get_formats : t -> format*format = "caml_gavl_vid_conv_get_formats"

  external get_quality : t -> int = "caml_gavl_vid_conv_get_quality"

  external set_quality : t -> int -> unit = "caml_gavl_vid_conv_set_quality"

  type int_rect = int*int*int*int 

  type float_rect = float*float*float*float

  external get_rect : t -> float_rect*int_rect = "caml_gavl_vid_conv_get_rectangle"

  external set_rect : t -> float_rect -> int_rect -> unit = "caml_gavl_vid_conv_set_rectangle"

  type conversion_flags = [
    | `Force_deinterlace
    | `Convolve_chroma
    | `Convolve_normalize
    | `Resample_chroma
  ]

  let int_of_conversion_flag x =
    match x with
      | `Force_deinterlace -> int_of_define "GAVL_FORCE_DEINTERLACE"
      | `Convolve_chroma -> int_of_define "GAVL_CONVOLVE_CHROMA"
      | `Convolve_normalize -> int_of_define "GAVL_CONVOLVE_NORMALIZE"
      | `Resample_chroma -> int_of_define "GAVL_RESAMPLE_CHROMA"

  let flags_of_conversion_flags l =
    let f x y = 
      let y = int_of_conversion_flag y in
      x lor y
    in
    List.fold_left f 0 l 

  external set_flags : t -> int -> unit = "caml_gavl_vid_conv_set_flags"

  let set_flags c l = 
    set_flags c (flags_of_conversion_flags l)

  external get_flags : t -> int = "caml_gavl_vid_conv_get_flags"

  let get_flags c = 
    let ret = get_flags c in
    let flags = [`Force_deinterlace;`Convolve_chroma;
                 `Convolve_normalize;`Resample_chroma]
    in
    let test_flag l f = 
      if ret land (int_of_conversion_flag f) <> 0 then
        f::l
      else
        l
    in
    List.fold_left test_flag [] flags

  type scale_mode = 
    | Auto
    | Nearest
    | Bilinear
    | Quadratic
    | Cubic_bspline
    | Cubic_mitchell
    | Cubic_catmull
    | Scale_sinc_lanczos

  let int_of_scale_mode x = 
    match x with
      | Auto -> int_of_define "GAVL_SCALE_AUTO"
      | Nearest -> int_of_define "GAVL_SCALE_NEAREST"
      | Bilinear -> int_of_define "GAVL_SCALE_BILINEAR"
      | Quadratic -> int_of_define "GAVL_SCALE_QUADRATIC"
      | Cubic_bspline -> int_of_define "GAVL_SCALE_CUBIC_BSPLINE"
      | Cubic_mitchell -> int_of_define "GAVL_SCALE_CUBIC_MITCHELL"
      | Cubic_catmull -> int_of_define "GAVL_SCALE_CUBIC_CATMULL"
      | Scale_sinc_lanczos -> int_of_define "GAVL_SCALE_SINC_LANCZOS"

  external set_scale_mode : t -> int -> unit = "caml_gavl_vid_conv_set_scale_mode"

  let set_scale_mode c m = 
    set_scale_mode c (int_of_scale_mode m)

  external get_scale_mode : t -> int = "caml_gavl_vid_conv_get_scale_mode"

  exception Internal of scale_mode

  let get_scale_mode c = 
    let ret = get_scale_mode c in
    let modes = [
      Auto;Nearest;Bilinear;Quadratic;
      Cubic_bspline;Cubic_mitchell;
      Cubic_catmull;Scale_sinc_lanczos]
    in
    let f m = 
      if ret = int_of_scale_mode m then
        raise (Internal m)
    in
    try
      List.iter f modes;
      assert(false);
    with
      | Internal m -> m

  external reinit : t -> unit = "caml_gavl_vid_conv_reinit"

  external convert : t -> frame -> frame -> unit = "caml_gavl_vid_conv_convert"

end

