with FFmpeg.avutil; use FFmpeg.avutil;

package FFmpeg.swscale is
   pragma Linker_Options ("-lswscale");

   type SwsContext is private;
   type SwsContext_Access is access SwsContext;
   pragma Convention (C, SwsContext_Access);

   type SwsVector is record
      coeff: double_access;
      length: C.int;
   end record;
   pragma Convention (C, SwsVector);
   type SwsVector_Access is access SwsVector;
   pragma Convention (C, SwsVector_Access);

   type SwsFilter is record
      lumH, lumV, chrH, chrV: SwsVector_Access;
   end record;
   pragma Convention (C, SwsFilter);
   type SwsFilter_Access is access SwsFilter;
   pragma Convention (C, SwsFilter_Access);

   function sws_alloc_context return SwsContext_Access;
   pragma Import (C, sws_alloc_context);

   function sws_init_context (
      sws_context: SwsContext_Access;
      srcFilter, dstFilter: SwsFilter_Access)
      return C.int;
   pragma Import (C, sws_init_context);

   SWS_FAST_BILINEAR : constant C.int := 16#001#;
   SWS_BILINEAR      : constant C.int := 16#002#;
   SWS_BICUBIC       : constant C.int := 16#004#;
   SWS_X             : constant C.int := 16#008#;
   SWS_POINT         : constant C.int := 16#010#;
   SWS_AREA          : constant C.int := 16#020#;
   SWS_BICUBLIN      : constant C.int := 16#040#;
   SWS_GAUSS         : constant C.int := 16#080#;
   SWS_SINC          : constant C.int := 16#100#;
   SWS_LANCZOS       : constant C.int := 16#200#;
   SWS_SPLINE        : constant C.int := 16#400#;

   function sws_getContext (
      srcW, srcH: C.int;
      srcFormat: PixelFormat;
      dstW, dstH: C.int;
      dstFormat: PixelFormat;
      flags: C.int;
      srcFilter: SwsFilter_Access;
      dstFilter: SwsFilter_Access;
      param: double_access)
      return SwsContext_Access; -- deprecated
   pragma Import (C, sws_getContext, "sws_getContext");

   function sws_scale (
      context: SwsContext_Access;
      --srcSlice: uint8_access_access;
      srcSlice: uint8_access_array; -- ???
      srcStride: int_array; -- XXX: Will passing an array work???
      srcSliceY: C.int;
      srcSliceH: C.int;
      --dst: uint8_access_access;
      dst: uint8_access_array; -- ???
      dstStride: int_array)
      return C.int;
   pragma Import (C, sws_scale);

   procedure sws_freeContext (swsContext: SwsContext_Access);
   pragma Import (C, sws_freeContext, "sws_freeContext");


private
   -- Not yet defined
   type SwsContext is null record;

end FFmpeg.swscale;
