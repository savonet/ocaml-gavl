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

module Video : 
sig

  exception Invalid_frame
  exception Invalid_conversion
  exception No_conversion_needed

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

  type plane = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type frame = 
  {
    planes               : (plane*int) array;
    timestamp            : Int64.t;
    duration             : Int64.t; 
    frame_interlace_mode : interlace_mode
  }

  val new_frame : format -> frame

  type converter

  val create_converter : format -> format -> converter

  val init : converter -> format -> format -> unit

  val get_formats : converter -> format*format

  val get_quality : converter -> int

  val set_quality : converter -> int -> unit

  type int_rect = int*int*int*int

  type float_rect = float*float*float*float

  val get_rect : converter -> float_rect*int_rect

  val set_rect : converter -> float_rect -> int_rect -> unit

  val reinit : converter -> unit

  val convert : converter -> frame -> frame -> unit

end

