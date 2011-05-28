package body FFmpeg.avutil is
   function av_strerror (errnum: Error_Code) return String
   is
      Buf: C.char_array (1 .. 100);
      Status: C.int;
   begin
      av_strerror (
         Status, 
         errnum,
         errbuf => Buf,
         errbuf_size => Buf'Length);
      if Status = 0 then
         return C.To_Ada (Buf);
      else
         return "(unknown error)";
      end if;
   end av_strerror;
end FFmpeg.avutil;
