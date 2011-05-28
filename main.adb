with FFmpeg.avcodec; use FFmpeg.avcodec;
with FFmpeg.avformat; use FFmpeg.avformat;
with FFmpeg.avutil; use FFmpeg.avutil;
with FFmpeg.swscale; use FFmpeg.swscale;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;

procedure Main is
   use FFmpeg;
   package C renames Interfaces.C;
   package SIO renames Ada.Streams.Stream_IO;

   function LStrip (S: String) return String
   is begin
      if S (S'First) = ' ' then
         return S (S'First+1 .. S'Last);
      else
         return S;
      end if;
   end LStrip;

   procedure Put_Line (
      F: in out SIO.File_Type;
      Line: String)
   is begin
      for I in Line'Range loop
         Character'Output (SIO.Stream (F), Line (I));
      end loop;
      Character'Output (SIO.Stream (F), ASCII.LF);
   end Put_Line;

   procedure Save_Frame (
      RGB_Frame: AVFrame_Access;
      width, height: C.int)
   is
      use type C.int;
      use type FFmpeg.uint8_access;
      F: SIO.File_Type;
      Src: uint8_access;

   begin
      SIO.Create (F, Name => "/tmp/testoutput.ppm");
      Put_Line (F, "P6");
      Put_Line (F, LStrip (width'Img) & " " & LStrip (height'Img));
      Put_Line (F, "255");

      for Y in 0 .. height-1 loop
         Src := RGB_Frame.all.data(0)
            + C.ptrdiff_t (Y * RGB_Frame.all.linesize(0));
         declare
            -- Pun a stream elements array over the buffer.
            -- We really don't want an extra copy.
            subtype SEAA is Ada.Streams.Stream_Element_Array
              (1 .. Ada.Streams.Stream_Element_Offset (width*3));
            Punned: SEAA;
            for Punned'Address use Src.all'Address;
            pragma Import (Ada, Punned);
         begin
            SIO.Write (F, Punned);
         end;
      end loop;
      SIO.Close (F);
   end Save_Frame;

   FFmpeg_Error: exception;
   Bad: exception;

   Context: AVFormatContext_Access; -- TODO: SBRM
   File_Name: aliased Interfaces.C.char_array
      := C.To_C ("/home/aoe/tmp/cumHbQfmYmc.flv" & ASCII.NUL);
   Status: FFmpeg.Error_Code;

   use type C.int;

   --type AVStream_Array is array (Positive range <>) of aliased AVStream;
   --package Stream_Pointers is new Interfaces.C.Pointers
   --  (Index => Positive,
   --   Element => AVStream,
   --   Element_Array => AVStream_Array,
   --   Default_Terminator => (others => <>));
   --Stream: Stream_Pointers.Pointer;

   Found_Stream: Boolean := False;
   Stream_N: C.int;

   Codec_Context: AVCodecContext_Access;
   Codec: AVCodec_Access;
   Frame: AVFrame_Access;
   RGB_Frame: AVFrame_Access;
   Out_Buffer_Size: C.unsigned;
   Out_Buffer: Void_Pointer; -- To hold the RGB pixels we want
   use type Void_Pointer;

   function Pun_To_AVPicture is new Ada.Unchecked_Conversion
     (AVFrame_Access, AVPicture_Access);

   Junk: C.int;
   pragma Unreferenced (Junk);

   Packet: aliased AVPacket;
   got_picture: aliased C.int;

   Scale_Context: SwsContext_Access;
   Frame_Number: Positive;

   procedure Cleanup is
   begin
      if Scale_Context /= null then
         sws_freeContext (Scale_Context);
         Scale_Context := null;
      end if;
      if RGB_Frame /= null then
         av_free (RGB_Frame.all'Address);
         RGB_Frame := null;
      end if;
      if Frame /= null then
         av_free (Frame.all'Address);
         Frame := null;
      end if;
      if Out_Buffer /= Null_Pointer then
         av_free (Out_Buffer);
         Out_Buffer := Null_Pointer;
      end if;
      if Codec_Context /= null then
         Junk := avcodec_close (Codec_Context);
         Codec_Context := null;
      end if;
      if Context /= null then
         av_close_input_file (Context);
         Context := null;
      end if;
   end Cleanup;

begin
   avcodec_register_all;
   av_register_all;

   av_open_input_file (
      Status => Status,
      ic_ptr => Context,
      filename => --Interfaces.C.Strings.To_Chars_Ptr (
         --File_Name'Unchecked_Access),
         File_Name,
      fmt => null,
      buf_size => 0,
      ap => null);
   if Status /= OK then
      raise FFmpeg_Error with "Could not open file: "
         & av_strerror (Status) & ".";
   end if;

   Status := av_find_stream_info (Context);
   if Status < OK then
      raise FFmpeg_Error with "Could not find stream information: "
         & av_strerror (Status) & ".";
   end if;

   Put_Line ("There are" & Context.all.nb_streams'Img & " streams.");

   Found_Stream := False;
   Find_First_Video_Stream:
   for I in Context.all.streams'First .. Context.all.streams'First + Integer (Context.all.nb_streams) - 1 loop
      if Context.all.streams(I).all.codec.all.codec_type = AVMEDIA_TYPE_VIDEO then
         Put_Line ("Stream" & I'Img & " is a video stream.");
         Stream_N := C.int (I);
         Found_Stream := True;
         Codec_Context := Context.all.streams(I).all.codec;
         exit Find_First_Video_Stream;
      end if;
   end loop Find_First_Video_Stream;

   if not Found_Stream then
      raise Bad with "No video stream found.";
   end if;

   Put_Line ("Video stream is" & Codec_Context.all.width'Img
      & " x" & Codec_Context.all.height'Img);

   -- Find and open the decoder for the video stream.
   Codec := avcodec_find_decoder (Codec_Context.all.codec_id);
   if Codec = null then
      raise FFmpeg_Error with "Unsupported codec!";
   end if;

   Status := avcodec_open (Codec_Context, Codec);
   if Status /= OK then
      raise FFmpeg_Error with "Could not open codec: "
         & av_strerror (Status) & ".";
   end if;

   Frame := avcodec_alloc_frame;
   if Frame = null then
      raise FFmpeg_Error with "Could not allocate frame.";
   end if;

   RGB_Frame := avcodec_alloc_frame;
   if RGB_Frame = null then
      raise FFmpeg_Error with "Could not allocate frame.";
   end if;

   Out_Buffer_Size := C.unsigned (avpicture_get_size (
      PIX_FMT_RGB24, Codec_Context.all.width, Codec_Context.all.height));
   Out_Buffer := av_malloc (Out_Buffer_Size);
   if Out_Buffer = Null_Pointer then
      raise FFmpeg_Error with "Could not allocate buffer.";
   end if;

   Junk := avpicture_fill (
      picture => Pun_To_AVPicture (RGB_Frame),
      ptr => Out_Buffer,
      pix_fmt => PIX_FMT_RGB24,
      width => Codec_Context.all.width,
      height => Codec_Context.all.height);

   -- Use deprecated function for now.
   Scale_Context := sws_getContext (
      srcW => Codec_Context.all.width,
      srcH => Codec_Context.all.height,
      srcFormat => Codec_Context.all.pix_fmt,
      dstW => Codec_Context.all.width,
      dstH => Codec_Context.all.height,
      dstFormat => PIX_FMT_RGB24,
      flags => SWS_BICUBIC, -- ... but we're not resizing
      srcFilter => null,
      dstFilter => null,
      param => NULL);
   if Scale_Context = null then
      raise FFmpeg_Error with "Could not create SwsContext.";
   end if;

   Frame_Number := 1;
   while av_read_frame (Context, Packet'Unchecked_Access) >= 0 loop
      if Packet.stream_index = Stream_N then
         if avcodec_decode_video2 (
            avctx => Codec_Context,
            picture => Frame,
            got_picture_ptr => got_picture'Unchecked_Access,
            avpkt => Packet'Unchecked_Access) < 0
         then
            raise FFmpeg_Error with "Error decoding video.";
         end if;

         if got_picture /= 0 then
            -- Convert frame to RGB
            Junk := sws_scale (
               context => Scale_Context,
               srcSlice => Frame.all.data,
               srcStride => Frame.all.linesize,
               srcSliceY => 0,
               srcSliceH => Codec_Context.all.height,
               dst => RGB_Frame.all.data,
               dstStride => RGB_Frame.all.linesize);
            -- Save the frame.
            Save_Frame (
               RGB_Frame, Codec_Context.all.width,
               Codec_Context.all.height);
            Frame_Number := Frame_Number + 1;
         end if;
      end if;
      -- Free the packet. 'Packet' doesn't get freed, only some of its members.
      -- I'm not sure if this means it uses the memory allocator for each
      -- frame, or what...
      av_free_packet (Packet'Unchecked_Access);
      if Frame_Number = 5 then
         exit;
      end if;
   end loop;

   Cleanup;
exception when others =>
   begin
      Cleanup;
   exception when others =>
      null;
   end;
   raise;
end Main;
