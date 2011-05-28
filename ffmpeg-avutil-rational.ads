package FFmpeg.avutil.rational is
   type AVRational is record
      num: C.int;
      den: C.int;
   end record;
   pragma Convention (C, AVRational);
end FFmpeg.avutil.rational;
