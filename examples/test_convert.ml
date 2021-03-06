
open Gavl.Video

let yuv = 
  {
    frame_width      = 640;
    frame_height     = 480;
    image_width      = 640;
    image_height     = 480;
    pixel_width      = 640;
    pixel_height     = 480;
    pixelformat      = Yuvj_420_p;
    frame_duration   = 0;
    timescale        = 0;
    framerate_mode   = Still;
    chroma_placement = Default;
    interlace_mode   = No_interlace
  }

let rgb = 
  {
    frame_width      = 320;
    frame_height     = 240;
    image_width      = 320;
    image_height     = 240;
    pixel_width      = 320;
    pixel_height     = 240;
    pixelformat      = Rgb_32;
    frame_duration   = 0;
    timescale        = 0;
    framerate_mode   = Still;
    chroma_placement = Default;
    interlace_mode   = No_interlace
  }

let () =
  let n = 100 in
  let inf  = new_frame yuv in
  clear_frame yuv inf;
  let outf = new_frame rgb in
  let conv = create_converter yuv rgb in
  Printf.printf "Converting %d frames..\n" n; flush_all ();
  for i = 0 to n do
    ignore (convert conv inf outf)
  done;
  Gc.full_major ()

