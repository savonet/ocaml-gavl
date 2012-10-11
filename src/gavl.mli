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

  (** OCaml API for the gavl audio/video library. *)

  (** Currently, only the video conversion part is available.
    * Audio API should follow very soon (contributors are welcome !) *)

module Video : 
sig

  exception Invalid_frame
  exception Invalid_conversion

  (** Raised when a feature is not yet implemented,
    * in particular pixel format with data other than
    * unsigned 8 bit integers. *)
  exception Not_implemented

  (** Interlace mode type *)
  type interlace_mode = 
   | No_interlace (** Progressive *) 
   | Top_first    (** Top field first *)
   | Bottom_first (** Bottom field first *)
   | Mixed        (** Use interlace_mode of the frames *)

  (** Pixel formats. Only formats using 8 bit unsigned integers are supported for now. *)
  type pixel_format = 
   | Gray_8       (** 8 bit gray, scaled 0x00..0xff *)
   | Gray_16      (** 16 bit gray, scaled 0x0000..0xffff *)
   | Gray_float   (** floating point gray, scaled 0.0..1.0 *)
   | Graya_16     (** 8 bit gray + alpha, scaled 0x00..0xff *)
   | Graya_32     (** 16 bit gray + alpha, scaled 0x0000..0xffff *)
   | Graya_float  (** floating point gray + alpha, scaled 0.0..1.0 *)
   | Rgb_15       (** 15 bit RGB. Each pixel is a uint16_t in native byte order. 
                    * Color masks are: for red: 0x7C00, for green: 0x03e0, for blue: 0x001f *)
   | Bgr_15       (** 15 bit BGR. Each pixel is a uint16_t in native byte order. 
                    * Color masks are: for red: 0x001f, for green: 0x03e0, for blue: 0x7C00 *)
   | Rgb_16       (** 16 bit RGB. Each pixel is a uint16_t in native byte order. 
                    * Color masks are: for red: 0xf800, for green: 0x07e0, for blue: 0x001f *)
   | Bgr_16       (** 16 bit BGR. Each pixel is a uint16_t in native byte order. 
                    * Color masks are: for red: 0x001f, for green: 0x07e0, for blue: 0xf800 *)
   | Rgb_24       (** 24 bit RGB. Each color is an uint8_t. Color order is RGBRGB *)
   | Bgr_24       (** 24 bit BGR. Each color is an uint8_t. Color order is BGRBGR *)
   | Rgb_32       (** 32 bit RGB. Each color is an uint8_t. Color order is RGBXRGBX, where X is unused *)
   | Bgr_32       (** 32 bit BGR. Each color is an uint8_t. Color order is BGRXBGRX, where X is unused *)
   | Rgba_32      (** 32 bit RGBA. Each color is an uint8_t. Color order is RGBARGBA *)
   | Rgb_48       (** 48 bit RGB. Each color is an uint16_t in native byte order. Color order is RGBRGB *)
   | Rgba_64      (** 64 bit RGBA. Each color is an uint16_t in native byte order. Color order is RGBARGBA *)
   | Rgb_float    (** float RGB. Each color is a float (0.0 .. 1.0) in native byte order. Color order is RGBRGB *)
   | Rgba_float   (** float RGBA. Each color is a float (0.0 .. 1.0) in native byte order. Color order is RGBARGBA *)
   | Yuy2         (** Packed YCbCr 4:2:2. Each component is an uint8_t. Component order is Y1 U1 Y2 V1 *)
   | Yuvy         (** Packed YCbCr 4:2:2. Each component is an uint8_t. Component order is U1 Y1 V1 Y2 *)
   | Yuva_32      (** Packed YCbCrA 4:4:4:4. Each component is an uint8_t. 
                    * Component order is YUVA. Luma and chroma are video scaled, alpha is 0..255. *)
   | Yuva_64      (** Packed YCbCrA 4:4:4:4. Each component is an uint16_t. 
                    * Component order is YUVA. Luma and chroma are video scaled, alpha is 0..65535. *)
   | Yuv_float    (** Packed YCbCr 4:4:4. Each component is a float. Luma is scaled 0.0..1.0, chroma is -0.5..0.5 *)
   | Yuva_float   (** Packed YCbCrA 4:4:4:4. Each component is a float. Luma is scaled 0.0..1.0, chroma is -0.5..0.5 *)
   | Yuv_420_p    (** Packed YCbCrA 4:4:4:4. Each component is an uint16_t. 
                    * Component order is YUVA. Luma and chroma are video scaled, alpha is 0..65535. *)
   | Yuv_422_p    (** Planar YCbCr 4:2:2. Each component is an uint8_t *)
   | Yuv_444_p    (** Planar YCbCr 4:4:4. Each component is an uint8_t *)
   | Yuv_411_p    (** Planar YCbCr 4:1:1. Each component is an uint8_t *)
   | Yuv_410_p    (** Planar YCbCr 4:1:0. Each component is an uint8_t *)
   | Yuvj_420_p   (** Planar YCbCr 4:2:0. Each component is an uint8_t, 
                    * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuvj_422_p   (** Planar YCbCr 4:2:2. Each component is an uint8_t, 
                    * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuvj_444_p   (** Planar YCbCr 4:4:4. Each component is an uint8_t, 
                    * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuv_444_p_16 (** 16 bit Planar YCbCr 4:4:4. Each component is an uint16_t in native byte order. *)
   | Yuv_422_p_16 (** 16 bit Planar YCbCr 4:2:2. Each component is an uint16_t in native byte order. *)

  (** Framerate mode *)
  type framerate_mode = 
   | Constant  (** Constant framerate *)
   | Variable  (** Variable framerate *)
   | Still     (** Still image        *)

  (** Chroma placement. *)
  type chroma_placement = 
   | Default (** MPEG-1/JPEG *)
   | Mpeg2   (** MPEG-2      *)
   | Dvpal   (** DV PAL      *)

  (** Video format. *)
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

  (** Plane data, represented as a big array of unsigned 8 bits ints. *)
  type plane = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** Frame type. *)
  type frame = 
  {
    planes               : (plane*int) array;
    timestamp            : Int64.t;
    duration             : Int64.t; 
    frame_interlace_mode : interlace_mode
  }

  (** Create a new frame. *)
  val new_frame : format -> frame

  (** Clear a frame. *)
  val clear_frame : format -> frame -> unit

  (** Opaque type for a converter. *)
  type t

  (** [in_format out_format] creates a converter 
    * converting from [in_format] to [out_format]. 
    *
    * Raises [Not_implemented] if input or output format
    * do not use unsigned 8 bit integers. *) 
  val create_converter : format -> format -> t

  (** [init conv in_format out_format]: initializes a converter with 
    * new input and output formats. 
    * 
    * Raises [Not_implemented] if input or output format
    * do not use unsigned 8 bit integers. *)
  val init : t -> format -> format -> unit

  (** [get_formats conv] returns a pair [(in_format,out_format)]. *)
  val get_formats : t -> format*format

  (** Get quality setting. *)
  val get_quality : t -> int

  (** Set quality setting. *)
  val set_quality : t -> int -> unit

  (** Type for integer rectangles: x/y offset and width/height. *)
  type int_rect = int*int*int*int

  (** Type for float rectangles: x/y offset and width/height. *)
  type float_rect = float*float*float*float

  (** [get_rect conv] returns a pair [(in_rect,out_rect)]. *)
  val get_rect : t -> float_rect*int_rect

  (** [set_rect conv in_rec out_rec] sets input and output rectangles. *)
  val set_rect : t -> float_rect -> int_rect -> unit

  (** Type for conversion flags. *)
  type conversion_flags = [
    | `Force_deinterlace
    | `Convolve_chroma
    | `Convolve_normalize
    | `Resample_chroma 
  ]

  (** Set conversion flags. *)
  val set_flags : t -> conversion_flags list -> unit

  (** Get conversion flags. *)
  val get_flags : t -> conversion_flags list

  (** Type for scale mode. *)
  type scale_mode =
    | Auto
    | Nearest
    | Bilinear
    | Quadratic
    | Cubic_bspline
    | Cubic_mitchell
    | Cubic_catmull
    | Scale_sinc_lanczos

  (** Set scale mode. *)
  val set_scale_mode : t -> scale_mode -> unit

  (** Get scale mode. *)
  val get_scale_mode : t -> scale_mode

  (** Reinitialize a converter. Should be called when a setting has changed. *)
  val reinit : t -> unit

  (** Perform frame conversion. 
    * 
    * Raises [Invalid_frame] if input or output frame doesn't match 
    * corresponding format.
    * 
    * Raises [Not_implemented] if input or output format
    * do not use unsigned 8 bit integers. *) 
  val convert : t -> frame -> frame -> unit

end

