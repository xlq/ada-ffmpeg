package FFmpeg.avutil is
   procedure av_strerror (
      Status: out C.int;
      errnum: Error_Code;
      --errbuf: C.Strings.chars_ptr;
      errbuf: in out C.char_array;
      errbuf_size: C.size_t);
   pragma Import (C, av_strerror);
   pragma Import_Valued_Procedure (av_strerror);

   type PixelFormat is new C.int;
   -- XXX: PixelFormat is actually a huge enum.
   -- I couldn't be bothered to include all the constants,
   -- so define them here as needed:
   PIX_FMT_RGB24: constant PixelFormat := 2;

   type PixelFormat_Access is access PixelFormat;
   pragma Convention (C, PixelFormat_Access);

   type AVSampleFormat is (
      AV_SAMPLE_FMT_NONE,
      AV_SAMPLE_FMT_U8,
      AV_SAMPLE_FMT_S16,
      AV_SAMPLE_FMT_S32,
      AV_SAMPLE_FMT_FLT,
      AV_SAMPLE_FMT_DBL);
   for AVSampleFormat use (
      AV_SAMPLE_FMT_NONE => -1,
      AV_SAMPLE_FMT_U8  => 0,
      AV_SAMPLE_FMT_S16 => 1,
      AV_SAMPLE_FMT_S32 => 2,
      AV_SAMPLE_FMT_FLT => 3,
      AV_SAMPLE_FMT_DBL => 4);
   pragma Convention (C, AVSampleFormat);

   type AVMediaType is (
      AVMEDIA_TYPE_UNKNOWN,
      AVMEDIA_TYPE_VIDEO,
      AVMEDIA_TYPE_AUDIO,
      AVMEDIA_TYPE_DATA,
      AVMEDIA_TYPE_SUBTITLE,
      AVMEDIA_TYPE_ATTACHMENT,
      AVMEDIA_TYPE_NB);
   for AVMediaType use (
      AVMEDIA_TYPE_UNKNOWN    => -1,
      AVMEDIA_TYPE_VIDEO      => 0,
      AVMEDIA_TYPE_AUDIO      => 1,
      AVMEDIA_TYPE_DATA       => 2,
      AVMEDIA_TYPE_SUBTITLE   => 3,
      AVMEDIA_TYPE_ATTACHMENT => 4,
      AVMEDIA_TYPE_NB         => 5);
   pragma Convention (C, AVMediaType);

   -- An easier to use version of av_strerror
   function av_strerror (errnum: Error_Code) return String;

   function av_malloc (size: C.unsigned) return Void_Pointer;
   pragma Import (C, av_malloc);

   procedure av_free (ptr: Void_Pointer);
   pragma Import (C, av_free);
   
end FFmpeg.avutil;
