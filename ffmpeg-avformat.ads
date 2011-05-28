with FFmpeg.avutil.rational; use FFmpeg.avutil.rational;
with FFmpeg.avcodec; use FFmpeg.avcodec;

package FFmpeg.avformat is
   pragma Linker_Options ("-lavformat");

   type AVClass is private;
   type AVClass_Access is access constant AVClass;
   pragma Convention (C, AVClass_Access);


   type AVCodecParserContext is private;
   type AVCodecParserContext_Access is access AVCodecParserContext;
   pragma Convention (C, AVCodecParserContext_Access);

   type AVMetadata is private;
   type AVMetadata_Access is access constant AVClass;
   pragma Convention (C, AVMetadata_Access);

   type AVIOContext is private;
   type AVIOContext_Access is access constant AVIOContext;
   pragma Convention (C, AVIOContext_Access);

   type AVProgram is private;
   type AVProgram_Access is access AVProgram;
   pragma Convention (C, AVProgram_Access);
   type AVProgram_Access_Access is access AVProgram_Access;
   pragma Convention (C, AVProgram_Access_Access);

   type AVChapter is private;
   type AVChapter_Access is access AVChapter;
   pragma Convention (C, AVChapter_Access);
   type AVChapter_Access_Access is access AVChapter_Access;
   pragma Convention (C, AVChapter_Access_Access);

   MAX_STREAMS: constant := 20;
   type AVStream;
   type AVStream_Access is access AVStream;
   pragma Convention (C, AVStream_Access);
   type AVStream_Access_Array is array
      (Natural range 0 .. MAX_STREAMS-1)
      of aliased AVStream_Access;
   pragma Convention (C, AVStream_Access_Array);

   type double_array is array (Positive range <>)
      of aliased C.double;
   pragma Convention (C, double_array);

   type int64_array is array (Positive range <>)
      of aliased int64_t;
   pragma Convention (C, int64_array);

   type AVFrac is record
      val, num, den: int64_t;
   end record;
   pragma Convention (C, AVFrac);

   MAX_STD_TIMEBASES: constant := 60*12+5;

   type AVStream_Info is record
      last_dts: int64_t;
      duration_gcd: int64_t;
      duration_count: C.int;
      duration_error: double_array (1..MAX_STD_TIMEBASES);
      codec_info_duration: int64_t;
   end record;
   pragma Convention (C, AVStream_Info);

   type AVStream_Info_Access is access AVStream_Info;
   pragma Convention (C, AVStream_Info_Access);

   type AVStreamParseType is (
      AVSTREAM_PARSE_NONE,
      AVSTREAM_PARSE_FULL,
      AVSTREAM_PARSE_HEADERS,
      AVSTREAM_PARSE_TIMESTAMPS,
      AVSTREAM_PARSE_FULL_ONCE);
   pragma Convention (C, AVStreamParseType);

   type AVIndexEntry is record
      pos: int64_t;
      timestamp: int64_t;
      flags: C.int range 0..3;
      size: C.int range 0..2**30-1;
      min_distance: C.int;
   end record;
   pragma Convention (C, AVIndexEntry);
   for AVIndexEntry use record
      pos at 0 range 0..63;
      timestamp at 8 range 0..63;
      flags at 16 range 0..1;
      size at 16 range 2..31;
      min_distance at 20 range 0..31;
   end record;
   type AVIndexEntry_Access is access AVIndexEntry;
   pragma Convention (C, AVIndexEntry_Access);

   type AVProbeData is record
      filename: C.Strings.chars_ptr;
      buf: Void_Pointer; -- XX: unsigned char *
      buf_size: C.int;
   end record;
   pragma Convention (C, AVProbeData);

   type AVPacketList;
   type AVPacketList_Access is access AVPacketList;
   pragma Convention (C, AVPacketList_Access);
   type AVPacketList is record
      pkt: AVPacket;
      next: AVPacketList_Access;
   end record;
   pragma Convention (C, AVPacketList);

   type AVInputFormat is private;
   type AVInputFormat_Access is access AVInputFormat;
   pragma Convention (C, AVInputFormat_Access);

   type AVOutputFormat is private;
   type AVOutputFormat_Access is access AVInputFormat;
   pragma Convention (C, AVOutputFormat_Access);

   MAX_REORDER_DELAY: constant := 16;

   type AVStream is record
      index: C.int;
      id: C.int;
      codec: AVCodecContext_Access;
      r_frame_rate: AVRational;
      priv_data: Void_Pointer;
      first_dts: int64_t;
      pts: AVFrac;
      time_base: AVRational;
      pts_wrap_bits: C.int;
      stream_copy: C.int;
      discard: AVDiscard;
      quality: C.C_float;
      start_time: int64_t;
      duration: int64_t;
      language: C.char_array (0..3);
      need_parsing: AVStreamParseType;
      parser: AVCodecParserContext_Access;
      cur_dts: int64_t;
      last_IP_duration: C.int;
      last_IP_pts: int64_t;
      index_entries: AVIndexEntry_Access;
      nb_index_entries: C.int;
      index_entries_allocated_size: C.unsigned;
      nb_frames: int64_t;
      unused: int64_array (1..4+1); -- deprecated
      filename: C.Strings.chars_ptr; -- deprecated
      disposition: C.int;
      probe_data: AVProbeData;
      pts_buffer: int64_array (1..MAX_REORDER_DELAY+1);
      sample_aspect_ratio: AVRational;
      metadata: AVMetadata_Access;
      cur_ptr: uint8_access;
      cur_len: C.int;
      cur_pkt: AVPacket;
      reference_data: int64_t;
      probe_packets: C.int;
      last_in_packet_buffer: AVPacketList_Access;
      avg_frame_rate: AVRational;
      codec_info_nb_frames: C.int;
      info: AVStream_Info_Access;
   end record;
   pragma Convention (C, AVStream);
   pragma Assert (AVStream'Size = 536 * Standard'Storage_Unit);

   type AVFormatContext is record
      av_class: AVClass_Access;
      iformat: AVInputFormat_Access;
      oformat: AVOutputFormat_Access;
      priv_data: Void_Pointer;
      pb: AVIOContext_Access;
      nb_streams: C.unsigned;
      streams: AVStream_Access_Array;
      filename: C.char_array (0..1023);
      timestamp: int64_t;
      title: C.char_array (0..511); -- deprecated
      author: C.char_array (0..511); -- deprecated
      copyright: C.char_array (0..511); -- deprecated
      comment: C.char_array (0..511); -- deprecated
      album: C.char_array (0..511); -- deprecated
      year: C.int; -- deprecated
      track: C.int; -- deprecated
      genre: C.char_array (0..31); -- deprecated
      ctx_flags: C.int; -- AVFMTCTX_xx
      packet_buffer: AVPacketList_Access;
      start_time: int64_t;
      duration: int64_t;
      file_size: int64_t;
      bit_rate: int64_t;
      cur_st: AVStream_Access;
      cur_ptr_deprecated: uint8_access; -- deprecated
      cur_len_deprecated: C.int; -- deprecated
      cur_pkt_deprecated: AVPacket; -- deprecated
      data_offset: int64_t;
      index_built: C.int; -- deprecated
      mux_rate: C.int;
      packet_size: C.unsigned;
      preload: C.int;
      max_delay: C.int;
      loop_output: C.int;
      flags: C.int;
      loop_input: C.int;
      probesize: C.unsigned;
      max_analyze_duration: C.int;
      key: uint8_access;
      keylen: C.int;
      nb_programs: C.unsigned;
      programs: AVProgram_Access_Access;
      video_codec_id: CodecID;
      audio_codec_id: CodecID;
      subtitle_codec_id: CodecID;
      max_index_size: C.unsigned;
      max_picture_buffer: C.unsigned;
      nb_chapters: C.unsigned;
      chapters: AVChapter_Access_Access;
      debug: C.int;
      raw_packet_buffer: AVPacketList_Access;
      raw_packet_buffer_end: AVPacketList_Access;
      packet_buffer_end: AVPacketList_Access;
      metadata: AVMetadata_Access;
      raw_packet_buffer_remaining_size: C.int;
      start_time_realtime: int64_t;
   end record;
   pragma Convention (C, AVFormatContext);
   pragma Assert (AVFormatContext'Size = 4144 * Standard'Storage_Unit);

   type AVFormatContext_Access is access AVFormatContext;
   pragma Convention (C, AVFormatContext_Access);

   type AVFormatParameters is private;

   --type Read_Probe_Func is access function (
   --   data: access AVProbeData)
   --   return C.int;
   --pragma Convention (C, Read_Probe_Func);

   --type Read_Header_Func is access function (
   --   context: access AVFormatContext;
   --   ap: access AVFormatParameters)
   --   return C.int;
   --pragma Convention (C, Read_Header_Func);

   --type AVInputFormat is record
   --   name: C.Strings.chars_ptr;
   --   long_name: C.Strings.chars_ptr;
   --   priv_data_size: C.int;
   --   read_probe: Read_Probe_Func;
   --   read_header: Read_Header_Func;

   procedure av_register_all;
   pragma Import (C, av_register_all);

   procedure av_open_input_file (
      Status: out Error_Code;
      ic_ptr: out AVFormatContext_Access;
      --File_Name: C.Strings.chars_ptr;
      filename: C.char_array;
      fmt: access AVInputFormat;
      buf_size: C.int;
      ap: access AVFormatParameters);
   pragma Import (C, av_open_input_file);
   pragma Import_Valued_Procedure (av_open_input_file);

   function av_find_stream_info (ic: AVFormatContext_Access)
      return Error_Code;
   pragma Import (C, av_find_stream_info);

   function av_read_frame (
      s: AVFormatContext_Access;
      pkt: AVPacket_Access)
      return C.int;
   pragma Import (C, av_read_frame);

   procedure av_close_input_file (s: AVFormatContext_Access);
   pragma Import (C, av_close_input_file);

private
   -- I haven't defined these yet.
   type AVClass is null record;
   type AVMetadata is null record;
   type AVIOContext is null record;
   type AVProgram is null record;
   type AVChapter is null record;
   type AVCodecParserContext is null record;
   type AVInputFormat is null record;
   type AVOutputFormat is null record;
   type AVFormatParameters is null record;
end FFmpeg.avformat;
