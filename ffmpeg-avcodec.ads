with FFmpeg.avutil; use FFmpeg.avutil;
with FFmpeg.avutil.rational; use FFmpeg.avutil.rational;

package FFmpeg.avcodec is
   pragma Linker_Options ("-lavcodec");

   procedure avcodec_register_all;
   pragma Import (C, avcodec_register_all);

   type AVPacket;
   type AVPacket_Access is access all AVPacket;
   pragma Convention (C, AVPacket_Access);

   subtype int_array_4 is int_array (1..4);

   type CodecID is new C.int;
   -- XXX: CodecID is really a gargantuan enumeration.

   type AVDiscard is (
      AVDISCARD_NONE,
      AVDISCARD_DEFAULT,
      AVDISCARD_NONREF,
      AVDISCARD_BIDIR,
      AVDISCARD_NONKEY,
      AVDISCARD_ALL);
   for AVDiscard use (
      AVDISCARD_NONE    => -16,
      AVDISCARD_DEFAULT => 0,
      AVDISCARD_NONREF  => 8,
      AVDISCARD_BIDIR   => 16,
      AVDISCARD_NONKEY  => 32,
      AVDISCARD_ALL     => 48);
   pragma Convention (C, AVDiscard);

   type AVPicture is record
      data: uint8_access_array (0..3);
      linesize: int_array (0..3);
   end record;
   pragma Convention (C, AVPicture);
   type AVPicture_Access is access AVPicture;
   pragma Convention (C, AVPicture_Access);

   type AVPanScan is private;
   type AVPanScan_Access is access AVPanScan;
   pragma Convention (C, AVPanScan_Access);

   type Motion_Val_Array_Inner is array (Natural range 0..1)
      of aliased int16_t;
   pragma Convention (C, Motion_Val_Array_Inner);
   type Motion_Val_Array_Inner_Access is access Motion_Val_Array_Inner;
   pragma Convention (C, Motion_Val_Array_Inner_Access);
   type Motion_Val_Array is array (Natural range 0..1)
      of aliased Motion_Val_Array_Inner_Access;

   type AVClass is private;
   type AVClass_Access is access AVClass;
   pragma Convention (C, AVClass_Access);

   type AVCodec is private;
   type AVCodec_Access is access AVCodec;
   pragma Convention (C, AVCodec_Access);

   type AVCodecContext;
   type AVCodecContext_Access is access AVCodecContext;
   pragma Convention (C, AVCodecContext_Access);

   type AVHWAccel is private;
   type AVHWAccel_Access is access AVHWAccel;
   pragma Convention (C, AVHWAccel_Access);

   type AVFrame is record
      data: uint8_access_array (0..3);
      linesize: int_array (0..3);
      base: uint8_access_array (0..3);
      key_frame: C.int;
      pict_type: C.int;
      pts: int64_t;
      coded_picture_number: C.int;
      display_picture_number: C.int;
      quality: C.int;
      age: C.int;
      reference: C.int;
      qscale_table: int8_access;
      qstride: C.int;
      mbskip_table: uint8_access;
      motion_val: Motion_Val_Array;
      mb_type: uint32_access;
      motion_subsample_log2: uint8_t;
      opaque: Void_Pointer;
      error: uint64_array (0..3);
      ttype: C.int;
      repeat_pict: C.int;
      qscale_type: C.int;
      interlaced_frame: C.int;
      top_field_first: C.int;
      pan_scan: AVPanScan_Access;
      palette_has_changed: C.int;
      buffer_hints: C.int;
      dct_coeff: short_access;
      ref_index: int8_access_array (0..1);
      reordered_opaque: int64_t;
      hwaccel_picture_private: Void_Pointer;
      pkt_pts: int64_t;
      pkt_dts: int64_t;
      owner: AVCodecContext_Access;
      thread_opaque: Void_Pointer;
      best_effort_timestamp: int64_t;
   end record;
   pragma Convention (C, AVFrame);
   pragma Assert (AVFrame'Size = 336 * System.Storage_Unit);
   type AVFrame_Access is access AVFrame;
   pragma Convention (C, AVFrame_Access);

   type AVColorPrimaries is (
      AVCOL_PRI_BT709,
      AVCOL_PRI_UNSPECIFIED,
      AVCOL_PRI_BT470M,
      AVCOL_PRI_BT470BG,
      AVCOL_PRI_SMPTE170M,
      AVCOL_PRI_SMPTE240M,
      AVCOL_PRI_FILM);
   for AVColorPrimaries use (
      AVCOL_PRI_BT709      => 1,
      AVCOL_PRI_UNSPECIFIED=> 2,
      AVCOL_PRI_BT470M     => 4,
      AVCOL_PRI_BT470BG    => 5,
      AVCOL_PRI_SMPTE170M  => 6,
      AVCOL_PRI_SMPTE240M  => 7,
      AVCOL_PRI_FILM       => 8);
   pragma Convention (C, AVColorPrimaries);

   type AVColorTransferCharacteristic is (
      AVCOL_TRC_BT709,
      AVCOL_TRC_UNSPECIFIED,
      AVCOL_TRC_GAMMA22,
      AVCOL_TRC_GAMMA28);
   for AVColorTransferCharacteristic use (
      AVCOL_TRC_BT709      => 1,
      AVCOL_TRC_UNSPECIFIED=> 2,
      AVCOL_TRC_GAMMA22    => 4,
      AVCOL_TRC_GAMMA28    => 5);
   pragma Convention (C, AVColorTransferCharacteristic);

   type AVColorSpace is (
      AVCOL_SPC_RGB,
      AVCOL_SPC_BT709,
      AVCOL_SPC_UNSPECIFIED,
      AVCOL_SPC_FCC,
      AVCOL_SPC_BT470BG,
      AVCOL_SPC_SMPTE170M,
      AVCOL_SPC_SMPTE240M);
   for AVColorSpace use (
      AVCOL_SPC_RGB        => 0,
      AVCOL_SPC_BT709      => 1,
      AVCOL_SPC_UNSPECIFIED=> 2,
      AVCOL_SPC_FCC        => 4,
      AVCOL_SPC_BT470BG    => 5,
      AVCOL_SPC_SMPTE170M  => 6,
      AVCOL_SPC_SMPTE240M  => 7);
   pragma Convention (C, AVColorSpace);

   type AVColorRange is (
      AVCOL_RANGE_UNSPECIFIED,
      AVCOL_RANGE_MPEG,
      AVCOL_RANGE_JPEG);
   for AVColorRange use (
      AVCOL_RANGE_UNSPECIFIED=> 0,
      AVCOL_RANGE_MPEG       => 1,
      AVCOL_RANGE_JPEG       => 2);
   pragma Convention (C, AVColorRange);

   type AVChromaLocation is (
      AVCHROMA_LOC_UNSPECIFIED,
      AVCHROMA_LOC_LEFT,
      AVCHROMA_LOC_CENTER,
      AVCHROMA_LOC_TOPLEFT,
      AVCHROMA_LOC_TOP,
      AVCHROMA_LOC_BOTTOMLEFT,
      AVCHROMA_LOC_BOTTOM);
   for AVChromaLocation use (
      AVCHROMA_LOC_UNSPECIFIED=> 0,
      AVCHROMA_LOC_LEFT       => 1,
      AVCHROMA_LOC_CENTER     => 2,
      AVCHROMA_LOC_TOPLEFT    => 3,
      AVCHROMA_LOC_TOP        => 4,
      AVCHROMA_LOC_BOTTOMLEFT => 5,
      AVCHROMA_LOC_BOTTOM     => 6);
   pragma Convention (C, AVChromaLocation);

   type AVLPCType is (
      AV_LPC_TYPE_DEFAULT,
      AV_LPC_TYPE_NONE,
      AV_LPC_TYPE_FIXED,
      AV_LPC_TYPE_LEVINSON,
      AV_LPC_TYPE_CHOLESKY);
   for AVLPCType use (
      AV_LPC_TYPE_DEFAULT     => -1,
      AV_LPC_TYPE_NONE        =>  0,
      AV_LPC_TYPE_FIXED       =>  1,
      AV_LPC_TYPE_LEVINSON    =>  2,
      AV_LPC_TYPE_CHOLESKY    =>  3);
   pragma Convention (C, AVLPCType);

   type AVAudioServiceType is (
      AV_AUDIO_SERVICE_TYPE_MAIN,
      AV_AUDIO_SERVICE_TYPE_EFFECTS,
      AV_AUDIO_SERVICE_TYPE_VISUALLY_IMPAIRED,
      AV_AUDIO_SERVICE_TYPE_HEARING_IMPAIRED,
      AV_AUDIO_SERVICE_TYPE_DIALOGUE,
      AV_AUDIO_SERVICE_TYPE_COMMENTARY,
      AV_AUDIO_SERVICE_TYPE_EMERGENCY,
      AV_AUDIO_SERVICE_TYPE_VOICE_OVER,
      AV_AUDIO_SERVICE_TYPE_KARAOKE);
   for AVAudioServiceType use (
      AV_AUDIO_SERVICE_TYPE_MAIN              => 0,
      AV_AUDIO_SERVICE_TYPE_EFFECTS           => 1,
      AV_AUDIO_SERVICE_TYPE_VISUALLY_IMPAIRED => 2,
      AV_AUDIO_SERVICE_TYPE_HEARING_IMPAIRED  => 3,
      AV_AUDIO_SERVICE_TYPE_DIALOGUE          => 4,
      AV_AUDIO_SERVICE_TYPE_COMMENTARY        => 5,
      AV_AUDIO_SERVICE_TYPE_EMERGENCY         => 6,
      AV_AUDIO_SERVICE_TYPE_VOICE_OVER        => 7,
      AV_AUDIO_SERVICE_TYPE_KARAOKE           => 8);
   pragma Convention (C, AVAudioServiceType);

   type Draw_Horiz_Band_Procedure is access procedure (
      s: AVCodecContext_Access;
      src: AVFrame_Access;
      offset: int_array_4;
      y, ttype, height: C.int);
   pragma Convention (C, Draw_Horiz_Band_Procedure);

   type RTP_Callback_Procedure is access procedure (
      avctx: AVCodecContext_Access;
      data: Void_Pointer;
      size: C.int;
      mb_nb: C.int);
   pragma Convention (C, RTP_Callback_Procedure);

   type Get_Buffer_Function is access function (
      c: AVCodecContext_Access;
      pic: AVFrame_Access)
      return C.int;
   pragma Convention (C, Get_Buffer_Function);

   type Release_Buffer_Procedure is access procedure (
      c: AVCodecContext_Access;
      pic: AVFrame_Access);
   pragma Convention (C, Release_Buffer_Procedure);

   type Get_Format_Function is access function (
      s: AVCodecContext_Access;
      fmt: PixelFormat_Access)
      return PixelFormat;
   pragma Convention (C, Get_Format_Function);

   type Reget_Buffer_Function is access function (
      c: AVCodecContext_Access;
      pic: AVFrame_Access)
      return C.int;
   pragma Convention (C, Reget_Buffer_Function);

   type Execute_Function_Inner_Func is access function (
      c2: AVCodecContext_Access;
      arg: Void_Pointer)
      return C.int;
   pragma Convention (C, Execute_Function_Inner_Func);

   type Execute_Function is access function (
      c: AVCodecContext_Access;
      func: Execute_Function_Inner_Func;
      arg2: Void_Pointer;
      ret: int_access;
      count: Interfaces.C.int;
      size: Interfaces.C.int)
      return Interfaces.C.int;
   pragma Convention (C, Execute_Function);

   type Execute2_Function_Inner_Func is access function (
      c2: AVCodecContext_Access;
      arg: Void_Pointer;
      jobnr: C.int;
      threadnr: C.int)
      return C.int;
   pragma Convention (C, Execute2_Function_Inner_Func);

   type Execute2_Function is access function (
      c: AVCodecContext_Access;
      func: Execute2_Function_Inner_Func;
      arg2: Void_Pointer;
      ret: int_access;
      count: Interfaces.C.int)
      return Interfaces.C.int;
   pragma Convention (C, Execute2_Function);

   type RcOverride is record
      start_frame: C.int;
      end_frame: C.int;
      qscale: C.int;
      quality_factor: C.C_float;
   end record;
   pragma Convention (C, RcOverride);
   type RcOverride_Access is access RcOverride;
   pragma Convention (C, RcOverride_Access);

   AVPALETTE_COUNT: constant := 256;
   type AVPaletteControl is record
      palette_changed: C.int;
      palette: unsigned_array (1..AVPALETTE_COUNT);
   end record;
   pragma Convention (C, AVPaletteControl);
   type AVPaletteControl_Access is access AVPaletteControl;
   pragma Convention (C, AVPaletteControl_Access);

   type AVCodecContext is record
      av_class: AVClass_Access;
      bit_rate: C.int;
      bit_rate_tolerance: C.int;
      flags: C.int; -- CODEC_FLAG_*
      sub_id: C.int;
      me_method: C.int;
      extradata: uint8_Access;
      extradata_size: C.int;
      time_base: AVRational;
      width, height: C.int;
      gop_size: C.int;
      pix_fmt: PixelFormat;
      rate_emu: C.int;
      draw_horiz_band: Draw_Horiz_Band_Procedure;
      sample_rate: C.int;
      channels: C.int;
      sample_fmt: AVSampleFormat;
      frame_size: C.int;
      frame_number: C.int;
      real_pict_num: C.int;
      ddelay: C.int;
      qcompress: C.C_float;
      qblur: C.C_float;
      qmin: C.int;
      qmax: C.int;
      max_qdiff: C.int;
      max_b_frames: C.int;
      b_quant_factor: C.C_float;
      rc_strategy: C.int; -- obsolete
      b_frame_strategy: C.int;
      hurry_up: C.int; -- deprecated
      codec: AVCodec_Access;
      priv_data: Void_Pointer;
      rtp_payload_size: C.int;
      rtp_callback: RTP_Callback_Procedure;
      mv_bits: C.int;
      header_bits: C.int;
      i_tex_bits: C.int;
      p_tex_bits: C.int;
      i_count: C.int;
      p_count: C.int;
      skip_count: C.int;
      misc_bits: C.int;
      frame_bits: C.int;
      opaque: Void_Pointer;
      codec_name: C.char_array (1..32);
      codec_type: AVMediaType;
      codec_id: CodecID;
      codec_tag: C.unsigned;
      workaround_bugs: C.int;
      luma_elim_threshold: C.int;
      chroma_elim_threshold: C.int;
      strict_std_compliance: C.int;
      b_quant_offset: C.C_float;
      error_recognition: C.int;
      get_buffer: Get_Buffer_Function;
      release_buffer: Release_Buffer_Procedure;
      has_b_frames: C.int;
      block_align: C.int;
      parse_only: C.int;
      mpeg_quant: C.int;
      stats_out: C.Strings.chars_ptr;
      stats_in: C.Strings.chars_ptr;
      rc_qsquish: C.C_float;
      rc_qmod_amp: C.C_float;
      rc_qmod_freq: C.int;
      rc_override: RcOverride_Access;
      rc_override_count: C.int;
      rc_eq: C.Strings.chars_ptr;
      rc_max_rate: C.int;
      rc_min_rate: C.int;
      rc_buffer_size: C.int;
      rc_buffer_aggressivity: C.C_float;
      i_quant_factor: C.C_float;
      i_quant_offset: C.C_float;
      rc_initial_cplx: C.C_float;
      dct_algo: C.int;
      lumi_masking: C.C_float;
      temporal_cplx_masking: C.C_float;
      spatial_cplx_masking: C.C_float;
      p_masking: C.C_float;
      dark_masking: C.C_float;
      idct_algo: C.int;
      slice_count: C.int;
      slice_offset: int_access;
      error_concealment: C.int;
      dsp_mask: C.unsigned;
      bits_per_coded_sample: C.int;
      prediction_method: C.int;
      sample_aspect_ratio: AVRational;
      coded_frame: AVFrame_Access;
      debug: C.int;
      debug_mv: C.int;
      error: uint64_array (1..4);
      mb_qmin: C.int;
      mb_qmax: C.int;
      me_cmp: C.int;
      me_sub_cmp: C.int;
      mb_cmp: C.int;
      ildct_cmp: C.int;
      dia_size: C.int;
      last_predictor_count: C.int;
      pre_me: C.int;
      me_pre_cmp: C.int;
      pre_dia_size: C.int;
      me_subpel_quality: C.int;
      get_format: Get_Format_Function;
      dtg_active_format: C.int;
      me_range: C.int;
      intra_quant_bias: C.int;
      inter_quant_bias: C.int;
      color_table_id: C.int;
      internal_buffer_count: C.int;
      internal_buffer: Void_Pointer;
      global_quality: C.int;
      coder_type: C.int;
      context_model: C.int;
      slice_flags: C.int;
      xvmc_acceleration: C.int;
      mb_decision: C.int;
      intra_matrix: uint16_access;
      inter_matrix: uint16_access;
      stream_codec_tag: C.unsigned;
      scenechange_threshold: C.int;
      lmin: C.int;
      lmax: C.int;
      palctrl: AVPaletteControl_Access;
      noise_reduction: C.int;
      reget_buffer: Reget_Buffer_Function;
      rc_initial_buffer_occupancy: C.int;
      inter_threshold: C.int;
      flags2: C.int;
      error_rate: C.int;
      antialias_algo: C.int;
      quantizer_noise_shaping: C.int;
      thread_count: C.int;
      execute: Execute_Function;
      thread_opaque: Void_Pointer;
      me_threshold: C.int;
      mb_threshold: C.int;
      intra_dc_precision: C.int;
      nsse_weight: C.int;
      skip_top: C.int;
      skip_bottom: C.int;
      profile: C.int;
      level: C.int;
      lowres: C.int;
      coded_width, coded_height: C.int;
      frame_skip_threshold: C.int;
      frame_skip_factor: C.int;
      frame_skip_exp: C.int;
      frame_skip_cmp: C.int;
      border_masking: C.C_float;
      mb_lmin: C.int;
      mb_lmax: C.int;
      me_penalty_compensation: C.int;
      skip_loop_filter: AVDiscard;
      skip_idct: AVDiscard;
      skip_frame: AVDiscard;
      bidir_refine: C.int;
      brd_scale: C.int;
      crf: C.C_float;
      cqp: C.int;
      keyint_min: C.int;
      refs: C.int;
      chromaoffset: C.int;
      bframebias: C.int;
      trellis: C.int;
      complexityblur: C.C_float;
      deblockalpha: C.int;
      deblockbeta: C.int;
      partitions: C.int;
      directpred: C.int;
      cutoff: C.int;
      scenechange_factor: C.int;
      mv0_threshold: C.int;
      b_sensitivity: C.int;
      compression_level: C.int;
      use_lpc: C.int;
      lpc_coeff_precision: C.int;
      min_prediction_order: C.int;
      max_prediction_order: C.int;
      prediction_order_method: C.int;
      min_partition_order: C.int;
      max_partition_order: C.int;
      timecode_frame_start: int64_t;
      request_channels: C.int;
      drc_scale: C.C_float;
      reordered_opaque: int64_t;
      bits_per_raw_sample: C.int;
      channel_layout: int64_t;
      request_channel_layout: int64_t;
      rc_max_available_vbv_use: C.C_float;
      rc_min_vbv_overflow_use: C.C_float;
      hwaccel: AVHWAccel_Access;
      ticks_per_frame: C.int;
      hwaccel_context: Void_Pointer;
      color_primaries: AVColorPrimaries;
      color_trc: AVColorTransferCharacteristic;
      colorspace: AVColorSpace;
      color_range: AVColorRange;
      chroma_sample_location: AVChromaLocation;
      execute2: Execute2_Function;
      weighted_p_pred: C.int;
      aq_mode: C.int;
      aq_strength: C.C_float;
      psy_rd: C.C_float;
      psy_trellis: C.C_float;
      rc_lookahead: C.int;
      crf_max: C.C_float;
      log_level_offset: C.int;
      lpc_type: AVLPCType;
      lpc_passes: C.int;
      slices: C.int;
      subtitle_header: uint8_access;
      subtitle_header_size: C.int;
      pkt: AVPacket_Access;
      is_copy: C.int;
      thread_type: C.int;
      active_thread_type: C.int;
      thread_safe_callbacks: C.int;
      vbv_delay: uint64_t;
      audio_service_type: AVAudioServiceType;
      pts_correction_num_faulty_pts: int64_t;
      pts_correction_num_faulty_dts: int64_t;
      pts_correction_last_pts: int64_t;
      pts_correction_last_dts: int64_t;
   end record;
   pragma Convention (C, AVCodecContext);
   pragma Assert (AVCodecContext'Size = 1192 * Standard'Storage_Unit);

   type Destruct_Procedure is access procedure (
      Packet: AVPacket_Access);
   pragma Convention (C, Destruct_Procedure);

   type AVPacket is record
      pts: int64_t;
      dts: int64_t;
      data: Void_Pointer; -- XX: uint8_t *
      size: C.int;
      stream_index: C.int;
      flags: C.int;
      duration: C.int;
      destruct: Destruct_Procedure;
      priv: Void_Pointer;
      pos: int64_t;
      convergence_duration: int64_t;
   end record;
   pragma Convention (C, AVPacket);
   pragma Assert (AVPacket'Size = 72 * Standard'Storage_Unit);

   function avcodec_find_decoder (id: CodecID)
      return AVCodec_Access;
   pragma Import (C, avcodec_find_decoder);

   function avcodec_open (
      avctx: AVCodecContext_Access;
      codec: AVCodec_Access)
      return Error_Code;
   pragma Import (C, avcodec_open);

   function avcodec_alloc_frame return AVFrame_Access;
   pragma Import (C, avcodec_alloc_frame);

   function avpicture_get_size (
      pix_fmt: PixelFormat;
      width: C.int;
      height: c.int)
      return C.int;
   pragma Import (C, avpicture_get_size);

   function avpicture_fill (
      picture: AVPicture_Access;
      --ptr: uint8_access;
      ptr: Void_Pointer;
      pix_fmt: PixelFormat;
      width: C.int;
      height: C.int)
      return C.int;
   pragma Import (C, avpicture_fill);

   function avcodec_decode_video2 (
      avctx: AVCodecContext_Access;
      picture: AVFrame_Access;
      got_picture_ptr: int_access;
      avpkt: AVPacket_Access)
      return C.int;
   pragma Import (C, avcodec_decode_video2);

   procedure av_free_packet (pkt: AVPacket_Access);
   pragma Import (C, av_free_packet);

   function avcodec_close (avctx: AVCodecContext_Access)
      return C.int;
   pragma Import (C, avcodec_close);

private
   -- These structures aren't defined yet.
   type AVHWAccel is null record;
   type AVCodec is null record;
   type AVClass is null record;
   type AVPanScan is null record;

end FFmpeg.avcodec;

